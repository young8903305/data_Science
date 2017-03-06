# read cmd and parse parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Error Command\nUSAGE: Rscript hw1_105753024.R -files test.1.csv -out result.csv", call.=FALSE)
} else  {
  i <- 1 
  while(i < length(args)){
    if(args[i] == "-files"){
      input_name <- args[i+1]
    }else if(args[i] == "-out"){
      output_name <- args[i+1]
    }
    i <- i + 1
  }
  if (!file.exists(input_name)) {
    stop("ERROR: No input file.")
  }
}

#===test arguments===
#input_name <- "test.1.csv"
#output_name <- "result.csv"

#===read file===
input <- read.table(input_name, header = TRUE, sep = ",", encoding = "UTF-8")

#===find each column's max and round it to which digit===
#=not user-defined function=
#weight <- round( max(input$weight), digits = 2)
#height <- round( max(input$height), digits = 2)

#=user-defined function=
round_max <- function(data, digit){
  r_max <- round( max(data), digits = digit)
  return (r_max)
}
weight <- round_max(input$weight, 2)
height <- round_max(input$height, 2)

set <- as.character(strsplit(input_name, split = ".csv"))
#===build a data frame===
output <- data.frame(set, weight, height)

#===write out the csv===
write.table(output, file = output_name, sep = ",", col.names = TRUE, row.names = FALSE)