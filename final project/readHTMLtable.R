library(XML)
library(RCurl)
library(httr)
library(stringr)
Sys.setlocale(category='LC_ALL', locale='C')

year <- 85
month <- 3
origin <- 'http://web.metro.taipei/RidershipCounts/c/'
if(month < 10){
  num <- paste0(year, '0', month)
}else{
  num <- paste0(year, month)
}
url <- paste0(origin, num, '.htm')
get_url_parse =htmlParse(url,encoding ='big5')
tablehead <- xpathSApply(get_url_parse, "//tr/td", xmlValue)
tablehead<-iconv(tablehead,"utf8","big5")
i <- 5
new_table <- c()
while(i <= length(tablehead)){
  if(!is.na(tablehead[i])){
    new_table <- c(new_table, tablehead[i])
  }
  i <- i+1
}
table <- as.data.frame(matrix(new_table, ncol = 2, byrow = T))
names(table) <- c('day', 'value')
write.csv(table,file = "test.csv" ,row.names = F)





year <- 85
month <- 3
origin <- 'http://web.metro.taipei/RidershipCounts/c/'

#input <- read.table('test.csv', header = TRUE, sep = ",", encoding = "big5")
input <- NULL
while(1){

if(year==106 && month==3){
  break
}

if(month > 12){
  year <- year + 1
  month <- 1
}
if(month < 10){
  num <- paste0(year, '0', month)
}else{
  num <- paste0(year, month)
}
month <- month + 1


##generate the list of url
url <- paste0(origin, num, '.htm')

get_url_parse =htmlParse(url,encoding ='big5')


tablehead <- xpathSApply(get_url_parse, "//tr/td", xmlValue)


tablehead<-iconv(tablehead,"utf8","big5")

i <- 5
new_table <- c()
while(i <= length(tablehead)){
  if(!is.na(tablehead[i])){
    new_table <- c(new_table, tablehead[i])
  }
  i <- i+1
}
if(i%%2==0){
  new_table <- c(new_table, " ")
}

#table <- matrix(new_table, ncol = 2, byrow = T)
table <- as.data.frame(matrix(new_table, ncol = 2, byrow = TRUE))
names(table) <- c('day', 'value')

input <- rbind.data.frame(input, table)
}
write.csv(input,file = "test.csv" ,row.names = F)



