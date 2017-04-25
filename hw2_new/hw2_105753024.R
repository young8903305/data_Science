library("ROCR")
# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_105753024.R --target male/female --files method1.csv method2.csv ... method10.csv --out out.csv", call.=FALSE)
}

# parse parameters
i <- 1
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m <- args[i+1]
    i <- i + 1
  }else if(args[i] == "--files"){
    j <- grep("-", c(args[ (i+1):length(args)], "-"))[1]
    files <- args[(i+1):(i+j-1)]
    i <- i + j - 1
  }else if(args[i] == "--out"){
    out_f <- args[i+1]
    i <- i + 1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i <- i + 1
}

#===test value===
#query_m <- "female"
#files <- c("method1.csv", "method2.csv", "method3.csv", "method4.csv", "method5.csv", "method6.csv", "method7.csv", "method8.csv", "method9.csv", "method10.csv")
#out_f <- "out2.csv"

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
  temp_sensi <- round(cal_sensitivity(data$prediction, data$reference, positive = query_m), digit = 2)
  temp_speci <- round(cal_specificity(data$prediction, data$reference, negative = reverse), digit = 2)
  preci <- cal_precision(data$prediction, data$reference, positive = query_m)
  temp_F1 <- round(2 * preci * temp_sensi / (preci + temp_sensi), digit = 2) #sensi = recall
#===calculate AUC===
  method_predict_ref <- NULL
  i <- 1
  while(i<=length(data$pred.score)){
    if(query_m=='female'){
      data$pred.score[i] <- 1 - data$pred.score[i]
    }
    if(query_m == data$reference[i]){
      method_predict_ref[i] <- 1
    }else{
      method_predict_ref[i] <- 0
    }
    i <- i + 1
  }
  eval <- prediction(data$pred.score, method_predict_ref)
  #plot(performance(eval,"tpr","fpr"))
  temp_AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
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
output <- rbind(output, last_row)

#===write out the result csv, write.csv is more suitable===
write.csv(output, file = out_f, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE)
print("It is finished")
