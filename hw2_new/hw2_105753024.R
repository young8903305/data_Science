library("ROCR")
# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_example.R --target male/female --files method1.csv method2.csv ... method10.csv --out out.csv", call.=FALSE)
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
#files <- c("method1.csv")
#out_f <- "out.csv"

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
  temp <- strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])] #deal with path
  name <- c(name, as.character(strsplit(temp, split = ".csv")))
  data <- read.table(file, header = TRUE, sep=",", encoding = "UTF-8")
#===calculate sensitivity, specificity, precision, F1-measure===
  temp_sensi <- cal_sensitivity(data$prediction, data$reference, positive = query_m)
  temp_speci <- cal_specificity(data$prediction, data$reference, negative = reverse)
  preci <- cal_precision(data$prediction, data$reference, positive = query_m)
  temp_F1 <- 2 * preci * temp_sensi / (preci + temp_sensi) #sensi = recall
#===calculate AUC===
  if(query_m == "female"){
    i <- 1
    while(i <= length(data$pred.score)){
      data$pred.score[i] <- 1 - data$pred.score[i] #because the origin predict score is for male
      i <- i + 1
    }
  }
  eval <- prediction(data$pred.score, data$reference)
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
last_row <- c("highest", name[which.max(sensitivity)], name[which.max(specificity)], name[which.max(F1)], name[which.max(AUC)])

#===create dataframe without last row, and combine them together=== 
output <- data.frame(name, sensitivity, specificity, F1, AUC, stringsAsFactors = FALSE)
output <- rbind(output, last_row)

#===write out the result csv===
write.table(output, file = out_f, sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)

print("It is finished")
