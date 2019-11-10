# IDSC_time_series
# Time series analysis V: ARIMA fitting and forecasting

#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)


#stock price

setwd("C:/Users/xxxxx/")
dat1 <- read.csv("dat1.csv")

summary(dat1)

plot.ts(dat1$price)

acf(dat1$price)
pacf(dat1$price) 

#Check stationarity
adf.test(dat1$price, alternative = "stationary")

#Differenciate
d1 = diff(dat1$price, differences = 1)
plot.ts(d1)

adf.test(d1, alternative = "stationary")

acf(d1)
pacf(d1) 

#MA(1)
fit1 <- arima(dat1$price, order = c(0,0,1))
fit1

f1 <- fitted(fit1)

plot.ts(dat1$price)
lines(f1, col="red")

#AR(1)
fit2 <- arima(dat1$price, order = c(1,0,0))
fit2

f2 <- fitted(fit2)

plot.ts(dat1$price)
lines(f2, col="red")

#ARMA(1,1)
fit3 <- arima(dat1$price, order = c(1,0,1))
fit3

f3 <- fitted(fit3)

plot.ts(dat1$price)
lines(f3, col="red")

#ARIMA(0,1,0)
fit4 <- arima(dat1$price, order = c(0,1,0))
fit4

f4 <- fitted(fit4)

plot.ts(dat1$price)
lines(f4, col="red")

#ARIMA(1,1,1)
fit5 <- arima(dat1$price, order = c(1,1,1))
fit5

f5 <- fitted(fit5)

plot.ts(dat1$price)
lines(f5, col="red")

#Compare AIC
fit1$aic
fit2$aic
fit3$aic
fit4$aic
fit5$aic

#Check residuals
tsdisplay(residuals(fit4), lag.max=15)

#Use autoarima
arimafit <- auto.arima(dat1$price, ic="aic", seasonal = T)
summary(arimafit)

#forecast
fcast <- forecast(fit4, h=20)
plot(fcast)


#Mycoplasma#

dat2 <- read.csv("dat2.csv")

summary(dat2)

plot.ts(dat2$case)

acf(dat2$case)
pacf(dat2$case) 

#Check stationarity
adf.test(dat2$case, alternative = "stationary")

#Differenciate
md1 = diff(dat2$case, differences = 1)
plot.ts(md1)
adf.test(md1, alternative = "stationary")

acf(md1)
pacf(md1)

#ARIMA(4,1,1)
mfit1 <- arima(dat2$case, order = c(4,1,1))
mfit1
mf1 <- fitted(mfit1)
plot.ts(dat2$case)
lines(mf1, col="red")

#ARIMA(6,1,1)
mfit2 <- arima(dat2$case, order = c(6,1,1))
mfit2
mf2 <- fitted(mfit2)
plot.ts(dat2$case)
lines(mf2, col="red")

#ARIMA(3,1,1)
mfit3 <- arima(dat2$case, order = c(3,1,1))
mfit3
mf3 <- fitted(mfit3)
plot.ts(dat2$case)
lines(mf3, col="red")

#AIC
mfit1$aic
mfit2$aic
mfit3$aic

#Check residuals
tsdisplay(residuals(mfit1), lag.max=15)

#Use autoarima
arimafit <- auto.arima(dat2$case, ic="aic", seasonal = T)
summary(arimafit)

#Forecast
fcast <- forecast(arimafit, h=20)
plot(fcast)


#Optional
yw = dat2$year*100 + dat2$week

start=201625

for(i in 0:20){
  ts = subset(dat2, yw<start+i)
  case.ts = ts(ts$case, start=c(2009,1), frequency=52) 
  arimafit <- auto.arima(case.ts, ic="aic", seasonal = F)
  fcast <- forecast(arimafit, h=6)
  plot(fcast, xlim=c(2015, 2017), ylim=c(0, 2.0), xlab="", ylab="cases", main="Mycoplasma forecasts using ARIMA model, Japan")
}

