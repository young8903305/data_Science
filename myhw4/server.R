library(shiny)
library("ggvis")

compute <- function(sex){
#===test value===
query_m <- sex
files <- c("method1.csv", "method2.csv", "method3.csv", "method4.csv", "method5.csv", "method6.csv", "method7.csv", "method8.csv", "method9.csv", "method10.csv")

#===calculate sensitivity===
cal_sensitivity <- function(data, reference, positive){
  TP <- sum(data == positive & reference == positive)
  TPFN <- sum(reference == positive)
  sen <- TP / TPFN
}

#===calculate specificity===
cal_specificity <- function(data, reference, negative){
  TN <- sum(data == negative & reference == negative)
  TNFP <- sum(reference == negative)
  spe <- TN / TNFP
}

#===calculate precision===
cal_precision <- function(data, reference, positive){
  TP <- sum(data == positive & reference == positive)
  TPFP <- sum(data == positive)
  pre <- TP / TPFP
}

#===initial value===
name <- c()
sensitivity <- c()
specificity <- c()
F1 <- c()
AUC <- c()
if(query_m == "female"){
  reverse <- "male"
}else{
  reverse <- "female"
}

#===read file and calculate each value===
for(file in files){
  name <- c(name, gsub(".csv", "", basename(file)))
  data <- read.table(file, header = TRUE, sep=",", encoding = "UTF-8")
  #===calculate sensitivity, specificity, precision, F1-measure, then round it===
  temp_sensi <- round(cal_sensitivity(tolower(data$prediction), tolower(data$reference), positive = query_m), digit = 2)
  temp_speci <- round(cal_specificity(tolower(data$prediction), tolower(data$reference), negative = reverse), digit = 2)
  preci <- cal_precision(tolower(data$prediction), tolower(data$reference), positive = query_m)
  temp_F1 <- round(2 * preci * temp_sensi / (preci + temp_sensi), digit = 2) #sensi = recall
  #===calculate AUC===
  method_predict_ref <- NULL
  i <- 1
  while(i<=length(data$pred.score)){
    if(query_m=='female'){
      data$pred.score[i] <- 1 - data$pred.score[i]
    }
    if(query_m == tolower(data$reference[i])){
      method_predict_ref[i] <- 1
    }else{
      method_predict_ref[i] <- 0
    }
    i <- i + 1
  }
  eval <- ROCR::prediction(data$pred.score, method_predict_ref)
  #plot(performance(eval,"tpr","fpr"))
  temp_AUC <- round(attributes(ROCR::performance(eval,'auc'))$y.values[[1]], digits = 2)
  #===concate them into one vector===
  sensitivity <- c(sensitivity, temp_sensi)
  specificity <- c(specificity, temp_speci)
  F1 <- c(F1, temp_F1)
  AUC <- c(AUC, temp_AUC)
}
#===a function to create confusion matrix easily, just to confirm my result===
#  c_matrix <- caret::confusionMatrix(data$prediction, data$reference, positive = query_m, mode = "everything")

#===the last row contain which is the highest method===
last_row <- c("highest", name[which.max(sensitivity)], name[which.max(specificity)], name[which.max(F1)], name[which.max(AUC)])

#===create dataframe without last row, and combine them together=== 
output <- data.frame(name, sensitivity, specificity, F1, AUC, stringsAsFactors = FALSE)
}

singleCompute <- function(fileIn){
  
  #===read file and calculate each value===
  data <- read.table(fileIn, header = TRUE, sep=",", encoding = "UTF-8")
  return(data)
}

shinyServer(function(input, output) {
  
  data <- reactive({ compute(input$sexual) })
  singleData <- reactive({ singleCompute(input$dataset) })

  data %>%
    ggvis(~1-specificity, ~sensitivity) %>%
    layer_points() %>%
    bind_shiny("plot", "plot_ui")
  
  output$table2 <- renderTable({
    head(singleData(), n = input$number)
  })
  
  output$table <- renderTable({
    data()
  })
  
})