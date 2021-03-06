library("ROCR")
# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_105753024.R --target male/female --files method1.csv method2.csv ... method10.csv --out out.csv", call.=FALSE)
}

# parse parameters
i <- 1
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m <- tolower(args[i+1])
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
#files <- c("hw3/data/set1/method1.csv", "hw3/data/set1/method2.csv", "hw3/data/set1/method3.csv", "hw3/data/set1/method4.csv", "hw3/data/set1/method5.csv", "hw3/data/set1/method6.csv", "hw3/data/set1/method7.csv", "hw3/data/set1/method8.csv", "hw3/data/set1/method9.csv", "hw3/data/set1/method10.csv")
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

#===find second largest value===
find_second <- function(data){
  s <- data
  s[which.max(s)] <- -1
  name[which.max(s)]
}

#===use Fisher's Exact Test to test significant===
significant <- function(input){
# find max and second max
  max <- name[which.max(input)]
  second <- find_second(input)
#read file  
  max_data <- read.csv(paste0(max, ".csv"), header = TRUE, sep=",", encoding = "UTF-8")
  second_data <- read.csv(paste0(second, ".csv"), header = TRUE, sep=",", encoding = "UTF-8")
#build data frame, table and do Fisher's test     
  data_temp <- rbind(
    data.frame(group=max, prediction=max_data$prediction),
    data.frame(group=second, prediction=second_data$prediction)
  )
  tab <- table(data_temp)
  fisher <- fisher.test(tab)
}

#===deal with the significant methodX to form like 'method*', others no change===
output_form <- function(input){
  result <- significant(input)
  if(result['p.value']<0.05){
    output <- paste0(name[which.max(input)], '*')
  }else{
    output <- name[which.max(input)]
  }
  return(output)
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
#===calculate sensitivity, specificity, precision, F1-measure===
  temp_sensi <- cal_sensitivity(tolower(data$prediction), tolower(data$reference), positive = query_m)
  temp_speci <- cal_specificity(tolower(data$prediction), tolower(data$reference), negative = reverse)
  preci <- cal_precision(tolower(data$prediction), tolower(data$reference), positive = query_m)
  temp_F1 <- (2 * preci * temp_sensi) / (preci + temp_sensi) #sensi = recall
#===calculate AUC===
  method_predict_ref <- NULL
  i <- 1
  while(i<=length(data$pred.score)){
    if(query_m == 'female'){
      data$pred.score[i] <- 1 - data$pred.score[i]
    }
    if(query_m == tolower(data$reference[i])){
      method_predict_ref[i] <- 1
    }else{
      method_predict_ref[i] <- 0
    }
    i <- i + 1
  }
  eval <- prediction(data$pred.score, method_predict_ref)
  #plot(performance(eval,"tpr","fpr"))
  temp_AUC <- attributes(performance(eval,'auc'))$y.values[[1]]
#===concate them into one vector===
  sensitivity <- c(sensitivity, temp_sensi)
  specificity <- c(specificity, temp_speci)
  F1 <- c(F1, temp_F1)
  AUC <- c(AUC, temp_AUC)
}

#===a function to create confusion matrix easily, just to confirm my result===
#  c_matrix <- caret::confusionMatrix(data$prediction, data$reference, positive = query_m, mode = "everything")

#===the last row contain which is the highest method===
last_row <- c("highest", output_form(sensitivity), output_form(specificity), output_form(F1), output_form(AUC))

#===all of them round to 2 digit===
sensitivity <- round(sensitivity, digits=2)
specificity <- round(specificity, digits=2)
F1 <- round(F1, digits=2)
AUC <- round(AUC, digits=2)

#===create dataframe without last row, and combine them together=== 
output <- data.frame(name, sensitivity, specificity, F1, AUC, stringsAsFactors = FALSE)
output <- rbind(output, last_row)

#===write out the result csv, write.csv is more suitable===
write.csv(output, file = out_f, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE)
print(paste("go to", dirname(out_f), "to checkout your output file"))

