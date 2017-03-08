# read cmd and parse parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Error Command\nUSAGE: Rscript hw2_105753024.R -files test.1.csv test.2.csv -out result.csv", call.=FALSE)
} else{
  i <- 1 # args array's index is begin from 1
  while(i < length(args)){
    if(args[i] == "-files"){
      input_name <- args[i+1]
      i <- i+1
      while(args[i+1] != "-out"){
        input_name <- c(input_name, args[i+1])
        i <- i+1
      }
    }
    output_name <- args[i+1]
    i <- i + 1
  }
  for(i in c(1:length(input_name)) ){
    if (!file.exists(input_name[i])) {
      stop("ERROR: No input file.")
    }
  }
} 

#===test arguments===
#input_name <- c("test.1.csv", "test.2.csv")
#output_name <- "result.csv"

#===initial each argument===
i <- 1  # index of input_name must begin from 1
weight <- NULL
height <- NULL
set <- NULL
#read file, find each file each column's max and round it, concate them into three different columns
round_max <- function(data, digit, result){
  result <- c(result, round( max(data), digits = digit))
  return (result)
}
while(i <= length(input_name)){
  input <- read.table(input_name[i], header = TRUE, sep = ",", encoding = "UTF-8")
  set <- c(set, as.character(strsplit(input_name[i], split = ".csv")))
  weight <- round_max(input$weight, 2, weight)
  height <- round_max(input$height, 2, height)
  i <- i+1
}

#===look for where is max===
id_w <- which.max(weight)
id_h <- which.max(height)

#===add max to each column===
set <- c(set, "max")
weight <- c(weight, as.character(strsplit(input_name[id_w], split = ".csv")))
height <- c(height, as.character(strsplit(input_name[id_h], split = ".csv")))

#===build the data frame===
output <- data.frame(set, weight, height)

#===write out the csv===
write.table(output, file = output_name, sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
