library("forecast")

# raw data from 1996.3 to 2017.2
all <- read.csv('data.csv', header = T, sep = ',', encoding = 'utf8')
value <- all$value
avevalue <- all$average
month <- all$month

# plot time series with month all value
valuetimeseries <- ts(value, frequency=12, start=c(1997,1))
plot.ts(valuetimeseries)

train<-window(valuetimeseries,start=c(1997,1),end=c(2015,12))
test<-window(valuetimeseries,start=c(2016,1),end=c(2016,12))

#decompose time series data
valuetimeseriescomponent <- decompose(valuetimeseries)
plot(valuetimeseriescomponent)

fit<-auto.arima(train)
fore <- forecast(fit, h=12)
plot(fore)
result <- accuracy(fore,test)

