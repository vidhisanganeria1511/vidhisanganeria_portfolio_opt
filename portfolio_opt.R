
library(shiny);library(quantmod);library(PerformanceAnalytics);
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Portfolio Optimization Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("stocks", "Enter Stock Tickers (comma separated)",
                    value = "AAPL,TSLA,MSFT"),
          
          dateInput("start", "Start Date",
                    value = "2020-01-01"),
          
          numericInput("n_portfolios", "Monte Carlo Portfolios",
                       value = 5000, min = 1000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("frontierPlot"), 
           verbatimTextOutput("portfolioStats")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dataInput <- reactive({
    
    tickers <- unlist(strsplit(input$stocks, ","))
    
    getSymbols(tickers,
               from = input$start,
               auto.assign = TRUE)
    
    prices <- do.call(merge,
                      lapply(tickers,
                             function(x) Ad(get(x))))
    
    returns <- na.omit(Return.calculate(prices))
    
    returns
  })
  
  monteCarlo <- reactive({
    
    returns <- dataInput()
    
    mean_returns <- colMeans(returns) * 252
    cov_matrix <- cov(returns) * 252
    
    num_assets <- ncol(returns)
    num_portfolios <- input$n_portfolios
    
    results <- matrix(0, nrow = 3, ncol = num_portfolios)
    
    for(i in 1:num_portfolios){
      
      weights <- runif(num_assets)
      weights <- weights / sum(weights)
      
      p_return <- sum(mean_returns * weights)
      
      p_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
      
      sharpe <- (p_return - 0.02) / p_vol
      
      results[1,i] <- p_return
      results[2,i] <- p_vol
      results[3,i] <- sharpe
    }
    
    data.frame(
      Return = results[1,],
      Volatility = results[2,],
      Sharpe = results[3,]
    )
  })
  
  output$frontierPlot <- renderPlot({
    
    df <- monteCarlo()
    
    ggplot(df, aes(x = Volatility, y = Return, color = Sharpe)) +
      geom_point(alpha = 0.6) +
      scale_color_gradient(low="blue", high="red") +
      theme_minimal() +
      ggtitle("Efficient Frontier")
    
  })
  
  output$portfolioStats <- renderPrint({
    
    df <- monteCarlo()
    
    best <- df[which.max(df$Sharpe),]
    
    cat("Optimal Portfolio:\n")
    print(best)
    
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
