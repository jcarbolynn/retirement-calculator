
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
    
    # Store full paths
    paths <- matrix(0, nrow = sims, ncol = total_months)
    
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
          balance <- balance + input$monthly_savings
        } else {
          balance <- balance - input$retirement_income / 12
        }
        
        if (balance <= 0) {
          balance <- 0
        }
        
        paths[s, m] <- balance
      }
      
      if (balance > 0) success <- success + 1
    }
    
    list(
      success_rate = success / sims,
      ending_balances = paths[, total_months],
      paths = paths
    )
  })
  
  optimizeSavings <- reactive({
    
    target <- input$target_success / 100
    
    # Search bounds
    lower <- 0
    upper <- 10000   # max monthly savings to test
    
    tolerance <- 0.005   # 0.5% accuracy in success rate
    max_iter <- 20       # binary search loops
    
    sims <- max(200, input$simulations)  # lower for speed during search
    
    best_solution <- NULL
    
    for (i in 1:max_iter) {
      
      test_savings <- (lower + upper) / 2
      
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
        
        if (balance > 0) success <- success + 1
      }
      
      success_rate <- success / sims
      
      # Track best result
      best_solution <- list(
        savings = test_savings,
        success = success_rate
      )
      
      # Converged
      if (abs(success_rate - target) < tolerance) {
        break
      }
      
      # Adjust search range
      if (success_rate < target) {
        lower <- test_savings   # need MORE savings
      } else {
        upper <- test_savings   # can try LESS savings
      }
    }
    
    # Final validation (optional but recommended)
    # Run full simulation at chosen savings level
    final_savings <- best_solution$savings
    
    final_mc <- isolate({
      
      success <- 0
      sims_full <- input$simulations
      
      total_months <- (input$life_exp - input$current_age) * 12
      retirement_month <- (input$retire_age - input$current_age) * 12
      
      for (s in 1:sims_full) {
        
        balance <- input$current_savings
        
        for (m in 1:total_months) {
          
          monthly_return <- rnorm(
            1,
            mean = (input$return_rate / 100) / 12,
            sd = (input$volatility / 100) / sqrt(12)
          )
          
          balance <- balance * (1 + monthly_return)
          
          if (m <= retirement_month) {
            balance <- balance + final_savings
          } else {
            balance <- balance - input$retirement_income / 12
          }
          
          if (balance <= 0) {
            balance <- 0
            break
          }
        }
        
        if (balance > 0) success <- success + 1
      }
      
      success / sims_full
    })
    
    list(
      savings = final_savings,
      success = final_mc
    )
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
    
    mc <- monteCarlo()
    
    paths <- mc$paths
    ages <- input$current_age + (1:ncol(paths)) / 12
    
    # median path
    median_path <- apply(paths, 2, median)
    
    plot_ly(
      x = ages,
      y = median_path,
      type = "scatter",
      mode = "lines",
      line = list(color = "blue"),
      name = "Median Path",
      hovertemplate = paste(
        "Age: %{x:.1f}<br>",
        "Balance: $%{y:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        title = "Median Monte Carlo Path",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Portfolio Value ($)"),
        
        shapes = list(
          list(
            type = "line",
            x0 = input$retire_age,
            x1 = input$retire_age,
            y0 = 0,
            y1 = max(median_path, na.rm = TRUE),
            line = list(color = "red", dash = "dash", width = 2)
          )
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