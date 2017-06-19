library(shiny)
library("ggvis")
library(plotly)
library("forecast")

all <- read.csv('data.csv', header = T, sep = ',', encoding = 'utf8')

readrawdata <- function(){
  return(all)
}

rawdata <- function(){
  # raw data from 1996.3 to 2017.2
  value <- all$value
  month <- all$month
  
  # plot time series with month all value
  valuetimeseries <- ts(value, frequency=12, start=c(1996,3))
}

traindata <- function(year){
# raw data from 1996.3 to 2017.2
#all <- read.csv('all4.csv', header = T, sep = ',', encoding = 'utf8')
value <- all$value
month <- all$month

# plot time series with month all value
valuetimeseries <- ts(value, frequency=12, start=c(1997,1))

train<-window(valuetimeseries,start=c(year[1],1),end=c(year[2],12))
return(train)
}

testdata <- function(year){
  #all <- read.csv('all4.csv', header = T, sep = ',', encoding = 'utf8')
  value <- all$value
  month <- all$month
  valuetimeseries <- ts(value, frequency=12, start=c(1997,1))

  test<-window(valuetimeseries,start=c(year[2]+1,1),end=c(year[2]+1,12))
} 

decomposedata <- function(year){
  value <- all$value
  month <- all$month
  
  # plot time series with month all value
  valuetimeseries <- ts(value, frequency=12, start=c(year[1],1),end=c(year[2]+1,12))
  #plot.ts(valuetimeseries)
  
  train<-window(valuetimeseries,start=c(year[1],1),end=c(year[2],12))
  test<-window(valuetimeseries,start=c(year[2]+1,1),end=c(year[2]+1,12))
  
  #decompose time series data
  valuetimeseriescomponent <- decompose(valuetimeseries)
}

ARIMA <- function(year){
  #all <- read.csv('all4.csv', header = T, sep = ',', encoding = 'utf8')
  value <- all$value
  month <- all$month
  
  # plot time series with month all value
  valuetimeseries <- ts(value, frequency=12, start=c(1997,1))
  
  train<-window(valuetimeseries,start=c(year[1],1),end=c(year[2],12))
  test<-window(valuetimeseries,start=c(year[2]+1,1),end=c(year[2]+1,12))
  
  fit<-auto.arima(train)
  fore <- forecast(fit, h=12)
}

ARIMAtest <- function(year){
  value <- all$value
  month <- all$month
  
  # plot time series with month all value
  valuetimeseries <- ts(value, frequency=12, start=c(1997,1))
  
  train<-window(valuetimeseries,start=c(year[1],1),end=c(year[2],12))
  test<-window(valuetimeseries,start=c(year[2]+1,1),end=c(year[2]+1,12))
  
  fit<-auto.arima(train)
  fore <- forecast(fit, h=12)
  result <- accuracy(fore,test)
  return(result)
}
#======================================================================


shinyServer(function(input, output) {
  
  output$raw <- renderPlot({
    plot(rawdata(), xlab = "Time", ylab = "value")
  })
  
  data <- reactive({ readrawdata() })
  output$rawtable <- renderTable({
    data()
  })
  
  output$train <- renderPlot({
    plot(traindata(input$year), xlab = "Time", ylab = "value")
  })
  
  output$test <- renderPlot({
    plot(testdata(input$year), xlab = "Time", ylab = "value")
  })
  
  output$decompose <- renderPlot({
    plot(decomposedata(input$year), xlab = "Time", ylab = "value")
  })
  
  output$ARIMA <- renderPlot({
    plot(ARIMA(input$year))
  })
  
  output$ARIMAtable <- renderTable({
    ARIMA(input$year)
  })
  
  output$accuracy <- renderTable({
    ARIMAtest(input$year)
  })
  
  
})