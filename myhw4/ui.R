library(shiny)
library("ggvis")
#shinyUI(fluidPage(
#  titlePanel("hw4_105753024"),
#    sidebarPanel(
#      radioButtons(inputId = "sexual", label = "Sexual predict choices", choices = list("male", "female")),
#      selectInput(inputId = "dataset", label = "Choose a mathod:", 
#                  choices = c("method 1"= "method1.csv", "method 2"= "method2.csv", "method 3"="method3.csv", "method 4"="method4.csv", "method 5"="method5.csv", "method 6"="method6.csv", "method 7"="method7.csv", "method 8"="method8.csv", "method 9"="method9.csv", "method 10"="method10.csv"), selected = "method1.csv"),
#      sliderInput(inputId = "number", label = "Number of person", min = 1, max = 39, value = 5),
      #numericInput("number", "Number of person to view:", 10),
#      uiOutput(outputId = "plot_ui")
#    ),
#    
#    mainPanel(
#      ggvisOutput("plot"),
#      tableOutput("table"),
#      tableOutput("table2")
#    )
#))

shinyUI(navbarPage("HW4_105753024",
      tabPanel("final result",
              h1("Show every method"),
              radioButtons(inputId = "sexual", label = "Sexual predict choices", choices = list("male", "female")),
              uiOutput(outputId = "plot_ui"),
              ggvisOutput("plot"),
              tableOutput("table")
      ),
                   
      tabPanel("raw data",
               h1("Each method's prediction"),
               selectInput(inputId = "dataset", label = "Choose a mathod:", 
                           choices = c("method 1"= "method1.csv", "method 2"= "method2.csv", "method 3"="method3.csv", "method 4"="method4.csv", "method 5"="method5.csv", "method 6"="method6.csv", "method 7"="method7.csv", "method 8"="method8.csv", "method 9"="method9.csv", "method 10"="method10.csv"), selected = "method1.csv"),
               sliderInput(inputId = "number", label = "Number of person", min = 1, max = 39, value = 5),
               tableOutput("table2")
      )            
))