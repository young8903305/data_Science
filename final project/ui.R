library(shiny)
library("ggvis")
library(ggplot2)

shinyUI(navbarPage("Final Project",
                   tabPanel(
                     "Train and Test",
                       h1("Show the timeseries for train data"),
                       plotOutput("train"),
                       sliderInput(inputId = "year", label = h5("slide year range for train"), min = 1997, 
                                 max = 2016, value = c(1997, 2000), width = "100%"),
                       h1("Show the timeseries for test data"),
                       plotOutput("test")
                     
                   ),
                   tabPanel("Decompose",
                           h1("Decompose the timeseries"),
                           plotOutput("decompose")
                   ),
                   tabPanel("ARIMA model",
                           h1("Show the model"),
                           plotOutput("ARIMA"),
                           h3("predict the value for next year each month"),
                           tableOutput("ARIMAtable"),
                           h3("show the accuracy"),
                           tableOutput("accuracy")
                   ),
                   tabPanel("Raw Data",
                            h1("Show the timeseries for raw data"),
                            plotOutput("raw"),
                            tableOutput("rawtable")
                   )
))