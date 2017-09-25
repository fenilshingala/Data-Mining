library(readr)
library(forecast)
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
dataTime = ts(data[,2],start = c(2003,1),frequency = 12)
plot(dataTime, xlab='Years', ylab = 'Tractor Sales')

require(forecast)
ARIMAfit = auto.arima(log10(dataTime), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 3)
# pred
plot(dataTime,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')