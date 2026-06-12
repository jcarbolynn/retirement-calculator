
library(shiny)
library(plotly)
# library(FinCal)
library(shinycssloaders)

ui <- fluidPage(
  titlePanel("Retirement Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("current_age", "Current Age:", 30),
      numericInput("retire_age", "Retirement Age:", 65),
      numericInput("life_exp", "Life Expectancy:", 90),
      
      hr(),
      
      numericInput("current_savings", "Current Savings ($):", 20000),
      numericInput("monthly_savings", "Monthly Contribution ($):", 500),
      numericInput("return_rate", "Expected Annual Return (%):", 6),
      
      hr(),
      
      numericInput("retirement_income", "Desired Annual Retirement Income ($):", 60000),
      
      hr(),
      
      numericInput("volatility", "Return Volatility (%):", value = 12, min = 0),
      numericInput("simulations", "Number of Simulations:", value = 200, min = 100),
      numericInput("target_success", "Target Success Rate (%)", value = 90, min = 50, max = 99)
      
    ),
    
    mainPanel(
      h3("Results"),
      withSpinner(verbatimTextOutput("summary")),
      withSpinner(plotlyOutput("savingsPlot")),
      withSpinner(plotlyOutput("mcPlot"))
    )
  )
)

server <- function(input, output) {
  
  # calcProjection <- reactive({
  #   
  #   years_to_retirement <- input$retire_age - input$current_age
  #   years_in_retirement <- input$life_exp - input$retire_age
  #   
  #   total_months <- years_to_retirement * 12
  #   monthly_rate <- input$return_rate / 100 / 12
  #   
  #   fv_current <- fv(
  #     pv = -input$current_savings,
  #     n = total_months,
  #     r = monthly_rate
  #   )
  #   
  #   fv_contrib <- fv.annuity(
  #     pmt = -input$monthly_savings,
  #     n = total_months,
  #     r = monthly_rate
  #   )
  #   
  #   required_fund <- pv.annuity(
  #     pmt = input$retirement_income,
  #     n = years_in_retirement,
  #     r = input$return_rate / 100
  #   )
  #   
  #   total_at_retirement <- fv_current + fv_contrib
  #   gap <- total_at_retirement - required_fund
  #   
  #   annual_withdrawal_possible <- pmt(
  #     pv = -total_at_retirement,  # starting balance
  #     fv = 0,                     # ending balance (0 for depletion)
  #     n = years_in_retirement,    # number of periods
  #     r = input$return_rate / 100 # interest rate
  #   )
  #   
  #   list(
  #     total = total_at_retirement,
  #     withdrawal = annual_withdrawal_possible,
  #     gap = gap
  #   )
  # })
  
  monteCarlo <- reactive({
    sims <- input$simulations
    total_months <- (input$life_exp - input$current_age) * 12
    retirement_month <- (input$retire_age - input$current_age) * 12
    
    mean_return <- input$return_rate / 100
    vol <- input$volatility / 100
    
    success <- 0
    ending_balances <- numeric(sims)
    
    for (s in 1:sims) {
      balance <- input$current_savings
      # random monthly returns
      for (m in 1:total_months) {
        monthly_return <- rnorm(
          1,
          mean = mean_return / 12,
          sd = vol / sqrt(12)
        )
        
        balance <- balance * (1 + monthly_return)
        
        if (m <= retirement_month) {
          balance <- balance + input$monthly_savings
        } else {
          balance <- balance - input$retirement_income / 12
        }
        
        if (balance <= 0) {
          balance <- 0
          break
        }
      }
      
      ending_balances[s] <- balance
      
      if (balance > 0) {
        success <- success + 1
      }
      
    }
    
    list(
      success_rate = success / sims,
      ending_balances = ending_balances
    )
  })
  
  optimizeSavings <- reactive({
    
    target <- input$target_success / 100
    test_savings <- input$monthly_savings
    step <- 100   # increase step size (adjustable)
    max_iter <- 50
    
    sims <- max(100, input$simulations)
    
    for (i in 1:max_iter) {
      
      # Run simulation with test savings
      total_months <- (input$life_exp - input$current_age) * 12
      retirement_month <- (input$retire_age - input$current_age) * 12
      
      mean_return <- input$return_rate / 100
      vol <- input$volatility / 100
      
      success <- 0
      
      for (s in 1:sims) {
        
        balance <- input$current_savings
        
        for (m in 1:total_months) {
          monthly_return <- rnorm(
            1,
            mean = mean_return / 12,
            sd = vol / sqrt(12)
          )
          
          balance <- balance * (1 + monthly_return)
          
          if (m <= retirement_month) {
            balance <- balance + test_savings
          } else {
            balance <- balance - input$retirement_income / 12
          }
          
          if (balance <= 0) {
            balance <- 0
            break
          }
        }
        
        if(!is.na(balance) && balance > 0) {
          success <- success + 1
        }
      }
      
      success_rate <- success / sims
      
      if (is.na(success_rate) || length(success_rate) == 0) {
        success_rate <- 0
      }

      if (success_rate >= target) {
        return(list(
          savings = test_savings,
          success = success_rate
        ))
      }
      
      # Otherwise increase savings
      test_savings <- test_savings + step
    }
    
    return(list(
      savings = NA,
      success = NA
    ))
  })
  
  output$summary <- renderText({
    
    mc <- monteCarlo()
    opt <- optimizeSavings()
    
    balances <- mc$ending_balances
    
    # Percentiles for context
    p10 <- quantile(balances, 0.10, na.rm = TRUE)
    p50 <- quantile(balances, 0.50, na.rm = TRUE)
    p90 <- quantile(balances, 0.90, na.rm = TRUE)
    
    # Handle optimizer result
    savings_text <- if (!is.null(opt$savings) && !is.na(opt$savings)) {
      paste0(
        "Monthly savings needed for ",
        input$target_success, "% success: $",
        round(opt$savings, 0),
        " (achieves ~", round(opt$success * 100, 1), "%)"
      )
    } else {
      "Target success rate not achievable within tested range"
    }
    
    paste0(
      "Monte Carlo Results:\n\n",
      "10th Percentile (downside): $", round(p10, 0), "\n",
      "Median Outcome: $", round(p50, 0), "\n",
      "90th Percentile (upside): $", round(p90, 0), "\n\n",
      
      "Probability of Success: ",
      round(mc$success_rate * 100, 1), "%\n\n",
      
      savings_text
    )
  })
  
  output$savingsPlot <- renderPlotly({
    
    total_months <- (input$life_exp - input$current_age) * 12
    retirement_month <- (input$retire_age - input$current_age) * 12
    
    balance <- numeric(total_months)
    current_balance <- input$current_savings
    
    monthly_rate <- input$return_rate / 100 / 12
    monthly_withdrawal <- input$retirement_income / 12
    
    for (m in 1:total_months) {
      
      # Apply growth first
      current_balance <- current_balance * (1 + monthly_rate)
      
      if (current_balance <= 0){
        balance[m] = 0
        break
      }
      
      if (m <= retirement_month) {
        # before retirement → contribute
        current_balance <- current_balance + input$monthly_savings
      } else {
        # after retirement → withdraw
        current_balance <- current_balance - monthly_withdrawal
      }
      balance[m] <- current_balance
    }
    
    ages <- input$current_age + (1:total_months) / 12
    
    plot_ly(
      x = ages,
      y = balance,
      type = "scatter",
      mode = "lines",
      line = list(color = "blue"),
      hovertemplate = paste(
        "Age: %{x:.1f}<br>",
        "Balance: $%{y:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        title = "Savings Lifecycle (Accumulation + Drawdown)",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Portfolio Value ($)")
      ) %>%
      layout(
        shapes = list(
          type = "line",
          x0 = input$retire_age,
          x1 = input$retire_age,
          y0 = 0,
          y1 = max(balance),
          line = list(dash = "dash", color = "red")
        )
      )
  })
  
  output$mcPlot <- renderPlotly({
    mc <- monteCarlo()
    
    plot_ly(
      x = mc$ending_balances,
      type = "histogram",
      nbinsx = 40,
      marker = list(color = "darkgreen")
    ) %>%
      layout(
        title = "Distribution of Ending Wealth",
        xaxis = list(title = "Final Portfolio Value"),
        yaxis = list(title = "Frequency")
      )
  })
}

shinyApp(ui = ui, server = server)