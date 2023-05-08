#########################################################
#  ECON 6210
#  Chris Kershaw and Sanjeev Ahuja
#  R Shiny Forecasting App
##########################################################


# load libraries
library(shiny)
library(fpp2)
library(quantmod)
library(shinythemes)
library(urca)

# load data from FRED
symbols <- c( "MRTSSM44112USN","MRTSSM44111USN","MRTSSM4413USN" )
m = length(symbols)

getSymbols(symbols,src="FRED")

USEDCARSALES <- MRTSSM44112USN
NEWCARSALES <- MRTSSM44111USN
CARPARTSALES <- MRTSSM4413USN


# Define UI 
ui <- fluidPage(
    theme = shinytheme("simplex"),
                pageWithSidebar(
    
    # Title
    headerPanel("R Shiny Web Forecasting App Project"),
    
    # Sidebar with controls to select the dataset and slider for forecast ahead duration
    sidebarPanel(
        # Select variable
        h6(selectInput("variable", "Variable:",
                    choices=c("USEDCARSALES", "NEWCARSALES","CARPARTSALES"))),
        h6(textOutput("text1")),
        br(),
        h6(sliderInput("ahead", "Periods to Forecast Ahead:",min=0, max=36, value= 12, step = 2)),
        h6(numericInput("start", "Starting year:", 2010)),
        
        submitButton("Update View"),
        br(),
        h6(p("Economic Forecasting App")),
        
    ),
    
    
    
    # Show the caption and forecast plots/tables
    mainPanel(
        h3(textOutput("caption")),
        tabsetPanel(
            tabPanel("ETS Forecast", plotOutput("etsForecastPlot"), verbatimTextOutput("etsForecastTable")), 
            tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"), verbatimTextOutput("arimaForecastTable")),
            tabPanel("TBATS Forecast", plotOutput("tbatsForecastPlot"), verbatimTextOutput("tbatsForecastTable")),
            tabPanel("Holt-Winters Additive Forecast", plotOutput("hwForecastPlot"), verbatimTextOutput("hwForecastTable")),
            tabPanel("Neural Network Autoregression Forecast", plotOutput("nnForecastPlot"), verbatimTextOutput("nnForecastTable")),
            tabPanel("Timeseries plot", plotOutput("tsPlot")),
            tabPanel("Unit Root Test (Test for Stationarity)", verbatimTextOutput ("unitroot")),
            tabPanel("Average forecasts", verbatimTextOutput("averageForecastTable"))
            
            
        )
    )
    
))



server <- (function(input, output) {
    
    getDataset <- reactive({
        data1 <- switch(input$variable,
                        USEDCARSALES = USEDCARSALES,
                        NEWCARSALES = NEWCARSALES,
                        CARPARTSALES = CARPARTSALES)
    })
    
    
    
    output$caption <- renderText({
        paste("Dataset: ", input$variable)
    })
    
    output$tsPlot <- renderPlot({
        
        y <- getDataset()
        date.start = input$start
        y   <-  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        p1 = autoplot(y) + ylab("Millions of Dollars") + ggtitle("Sales Over Time") + geom_line(col="green", size=1.5) +theme(plot.title = element_text(size=14, face = "bold") ,
                                                                                                                              axis.text.x = element_text(size=14),
                                                                                                                              axis.text.y = element_text(size=14)) 
        p2 = ggAcf(y) + ggtitle("ACF")
        gridExtra::grid.arrange(p1, p2, nrow=2)
    })
    
   
    output$unitroot <- renderPrint({
      
      y <- getDataset()
      date.start = input$start
      y   <-  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      u <- ur.kpss(y) 
      print(summary(u))

    })
    
    output$arimaForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- auto.arima(y)
        autoplot(forecast(fit, h=input$ahead))+ ylab("Millions of Dollars")
    })
    
    output$arimaForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- auto.arima(y)
        forecast(fit, h=input$ahead)
    })
    
    
    output$hwForecastPlot <- renderPlot({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- hw(y,seasonal="additive", h=36)
      autoplot(forecast(fit, h=input$ahead))+ ylab("Millions of Dollars")
    })
    
    output$hwForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- hw(y,seasonal="additive",h=36)
      forecast(fit, h=input$ahead)
    })
    
    output$nnForecastPlot <- renderPlot({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- nnetar(y, lambda=0)
      autoplot(forecast(fit, h=input$ahead))+ ylab("Millions of Dollars")
    })
    
    output$nnForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
      fit <- nnetar(y,lamda=0)
      forecast(fit, h=input$ahead)
    })
   
    output$etsForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- ets(y)
        autoplot(forecast(fit, h=input$ahead))+ ylab("Millions of Dollars")
    })
    
    output$etsForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- ets(y)
        forecast(fit, h=input$ahead)
    })
    
    output$tbatsForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- tbats(y)
        autoplot(forecast(fit, h=input$ahead))+ ylab("Millions of Dollars")
    })
    
    output$tbatsForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- tbats(y)
        forecast(fit, h=input$ahead)
        
    })
    
    output$averageForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit1 <- ets(y)
        fc1 = forecast(fit1, h=input$ahead)
        
        fit2 = auto.arima(y)
        fc2 = forecast(fit2, h=input$ahead)
        
        fit3 <- hw(y,seasonal="additive",h=36)
        fc3 <- forecast(fit3, h=input$ahead)
        
        fit4 <- nnetar(y,lamda=0)
        fc4 <- forecast(fit4, h=input$ahead)
        
        fit5 <- tbats(y)
        fc5 <- forecast(fit5, h=input$ahead)
        
        fc = (fc1$mean + fc2$mean + fc3$mean + fc4$mean + fc5$mean )/5
        fc
    })
    
    
    
    output$text1 <- renderText({
        
        switch(input$variable,
               USEDCARSALES = "US monthly sales from used car dealers",
               NEWCARSALES = "US monthly sales from new car dealers",
               CARPARTSALES = "US monthly sales from automotive parts, accessory, and tire stores")
    })
    
})

        
# Run the application 
shinyApp(ui = ui, server = server)
