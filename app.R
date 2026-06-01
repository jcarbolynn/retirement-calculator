#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# install.packages("fincal")
# install.packages("plotly")
library(shiny)
library(FinCal)
library(plotly)

years.w <- 32
years.r <- 30
current.income <- 40000
income.replacement <- 1
current.savings <- 200000

income.in.retirement <- (current.income * (1.025 ^ years.w) * income.replacement)
retirement.requirement <- pv(r = .04, n = years.r, pmt = income.in.retirement, type = 1)
monthly.savings <- pmt(r = .07/12, n = (years.w*12), pv = current.savings, fv = retirement.requirement, type = 1)
savings <- list(monthly.savings, -retirement.requirement)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Retirement Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("current_age", "Current Age: ", value = 30, min = 18, max = 100),
            numericInput("retire_age", "Retirement Age: ", value = 65, min = 40, max = 100),
            numericInput("life_exp", "Life Expectancy: ", value = 90, min = 60, max = 110),
            
            hr(),
            
            numericInput("current_savings", "Current Savings ($): ", value = 20000),
            numericInput("monthly_savings", "Monthly Contribution ($): ", value = 500),
            numericInput("return_rate", "Expected Annual Return (%): ", value = 6),
            
            hr(),
            
            numericInput("retirement_income", "Desired Annual Retirement Income ($): ", value = 60000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Results"),
           verbatimTextOutput("summary"),
           plotlyOutput("savingsPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    calcProjection <- reactive({
      years_to_retirement <- input$retire_age - input$current_age
      years_in_retirement <- input$life_exp - input$retire_age
      
      monthly_rate <- input$return_rate / 100 / 12
      total_months <- years_to_retirement * 12
      
      # future value of savings
      fv_current <- input$current_savings * (1 + monthly_rate)^(total_months)
      
      # future value of monthly contributions
      fv_contrib <- input$monthly_savings * (
        ((1 + monthly_rate)^total_months - 1) / monthly_rate
      )
      
      total_at_retirement <- fv_current + fv_contrib
      
      annual_withdrawl_possible <- total_at_retirement / years_in_retirement
      
      list(
        total = total_at_retirement,
        withdrawl = annual_withdrawl_possible,
        gap = annual_withdrawl_possible - input$retirement_income,
        years_to_retirement = years_to_retirement
      )
      
    })
    
    output$summary <- renderText({

      res <- calcProjection()
      
      paste0(
        "Projected savings at retirement: $", round(res$total, 0), "\n",
        "Estimated annual income available: $", round(res$withdrawl, 0), "\n",
        "Income gap / surplus: $", round(res$gap, 0)
      )
    })
    
    output$savingsPlot <- renderPlotly({
      years = seq(input$current_age, input$retire_age)
      values = numeric(length(years))
      
      target = input$retirement_income * (input$life_exp - input$retire_age)
      
      balance = input$current_savings
      monthly_rate = input$return_rate / 100 / 12
      
      for(i in seq_along(years)) {
        for(m in 1:12) {
          balance <- balance * (1 + monthly_rate) + input$monthly_savings
        }
        values[i] <- balance
      }
      
      plot_ly(
        x = years,
        y = values,
        type = "scatter",
        mode = "lines",
        line = list(color = "blue"),
        hovertemplate = paste(
          "Age: %{x}<br>",
          "Savings: %{y:,.0f}<extra></extra>"
        )
      ) %>%
        add_lines(y = rep(target, length(years)),
                  name = "Target Savings",
                  line = list(color = "red", dash = "dash")) %>%
        layout(
          title = "Projected Savings Growth",
          xaxis = list(title = "Age"),
          yaxis = list(title = "Savings ($)")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
