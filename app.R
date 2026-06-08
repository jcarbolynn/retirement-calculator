
library(shiny)
library(plotly)
library(FinCal)

ui <- fluidPage(
  titlePanel("Retirement Calculator (Correct FinCal Version)"),
  
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
      
      numericInput("retirement_income", "Desired Annual Retirement Income ($):", 60000)
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("summary"),
      plotlyOutput("savingsPlot")
    )
  )
)

server <- function(input, output) {
  
  calcProjection <- reactive({
    
    years_to_retirement <- input$retire_age - input$current_age
    years_in_retirement <- input$life_exp - input$retire_age
    
    total_months <- years_to_retirement * 12
    monthly_rate <- input$return_rate / 100 / 12
    
    fv_current <- fv(
      pv = -input$current_savings,
      n = total_months,
      r = monthly_rate
    )
    
    fv_contrib <- fv.annuity(
      pmt = -input$monthly_savings,
      n = total_months,
      r = monthly_rate
    )
    
    total_at_retirement <- fv_current + fv_contrib
    
    annual_withdrawal_possible <- total_at_retirement / years_in_retirement
    gap <- annual_withdrawal_possible - input$retirement_income
    
    list(
      total = total_at_retirement,
      withdrawal = annual_withdrawal_possible,
      gap = gap
    )
  })
  
  output$summary <- renderText({
    res <- calcProjection()
    
    paste0(
      "Projected savings at retirement: $", round(res$total, 0), "\n",
      "Estimated annual income available: $", round(res$withdrawal, 0), "\n",
      "Income gap / surplus: $", round(res$gap, 0)
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
}

shinyApp(ui = ui, server = server)