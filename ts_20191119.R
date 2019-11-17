#20191119 "Time series analysis: Confidence interval and prediction interval"

library("forecast")
library("tsModel")

usflu <- read.csv("usflu.csv")
flu <- usflu

summary(flu$fludeaths)
hist(flu$fludeaths)

m <- mean(flu$fludeaths)
sd <- sd(flu$fludeaths)
n <- length(flu$fludeaths)
se <- sd*sqrt(1/n)
se.p <- sd*sqrt(1+(1/n))

stripchart(flu$fludeaths, method = "jitter", jitter=0.01, vert=T, pch=19, xlim = c(0.9, 1.1), ylim=c(450,1500))
points(1.05, m, pch=19, col="blue")
arrows(1.05, m-1.96*se, 1.05, m+1.96*se, code=3, angle=90, length=.1, col="blue")
points(0.95, m, pch=19, col="red")
arrows(0.95, m-1.96*se.p, 0.95, m+1.96*se.p, code=3, angle=90, length=.1, col="red")

#Divide US flu dataset: -2014/15 season and 2015/16 season
flu$t <- seq.int(nrow(flu))

train <- subset(flu, fluyr<2015)
test <- subset(flu, fluyr>=2015)

plot(flu$fludeaths, type="n", xlim=c(1,311), ylim=c(0,1400), main = "Weekly flu deaths in US", xlab="Year", ylab="Flu deaths", xaxt="n")
rect(311, 0, 247, 1400, col=grey(0.9), border=F)
points(train$t, train$fludeaths, pch=20)
points(test$t, test$fludeaths, pch=1)
axis(1, at=0:5*52+12, labels=2011:2016)

#Linear regression
fit1 <- lm(fludeaths ~ t, data = train)
pred1.1 <- predict(fit1, data = train, interval="confidence", level=0.95)
train$fit <- pred1.1[,1]
train$lwr <- pred1.1[,2]
train$upr <- pred1.1[,3]

lines(train$t, train$fit, col="blue", lwd=2)
lines(train$t, train$upr, col="blue", lwd=1)
lines(train$t, train$lwr, col="blue", lwd=1)

pred1.2 <- predict(fit1, newdata = test, interval="predict", level=0.95)
test$fit <- pred1.2[,1]
test$lwr <- pred1.2[,2]
test$upr <- pred1.2[,3]

lines(test$t, test$fit, col="red", lwd=2)
lines(test$t, test$upr, col="red", lwd=1)
lines(test$t, test$lwr, col="red", lwd=1)

#Linear regression with one Fourier term (proposed by Serfling)

fit2 <- lm(fludeaths ~ t + sinpi(2*t/52) + cospi(2*t/52), data = train)

pred2.1 <- predict(fit2, data = train, interval="confidence", level=0.95)
train$fit <- pred2.1[,1]
train$lwr <- pred2.1[,2]
train$upr <- pred2.1[,3]

plot(flu$fludeaths, type="n", xlim=c(1,311), ylim=c(0,1400), main = "Weekly flu deaths in US", xlab="Year", ylab="Flu deaths", xaxt="n")
rect(311, 0, 247, 1400, col=grey(0.9), border=F)
points(train$t, train$fludeaths, pch=20)
points(test$t, test$fludeaths, pch=1)
axis(1, at=0:5*52+12, labels=2011:2016)

lines(train$t, train$fit, col="blue", lwd=2)
lines(train$t, train$upr, col="blue", lwd=1)
lines(train$t, train$lwr, col="blue", lwd=1)

pred2.2 <- predict(fit2, newdata = test, interval="predict", level=0.95)
test$fit <- pred2.2[,1]
test$lwr <- pred2.2[,2]
test$upr <- pred2.2[,3]

lines(test$t, test$fit, col="red", lwd=2)
lines(test$t, test$upr, col="red", lwd=1)
lines(test$t, test$lwr, col="red", lwd=1)

#SARIMA model
ts <- ts(train$fludeaths, frequency=52)
arimafit <- auto.arima(ts, seasonal = T)
summary(arimafit)

n <- length(train$fludeaths)
sd <- sqrt(arimafit$sigma2)
se <- sd*sqrt(1/n)

train$fit <- fitted(arimafit)
train$upr <- fitted(arimafit) + 1.96*se
train$lwr <- fitted(arimafit) - 1.96*se

plot(flu$fludeaths, type="n", xlim=c(1,311), ylim=c(0,1400), main = "Weekly flu deaths in US", xlab="Year", ylab="Flu deaths", xaxt="n")
rect(311, 0, 247, 1400, col=grey(0.9), border=F)
points(train$t, train$fludeaths, pch=20)
points(test$t, test$fludeaths, pch=1)
axis(1, at=0:5*52+12, labels=2011:2016)

lines(train$t, train$fit, col="blue", lwd=2)
lines(train$t, train$upr, col="blue", lwd=1)
lines(train$t, train$lwr, col="blue", lwd=1)

pred = predict(arimafit, n.ahead = 65)

test$fit <- pred$pred
test$upr <- pred$pred + 1.96*pred$se
test$lwr <- pred$pred - 1.96*pred$se

lines(test$t, test$fit, col="red", lwd=2)
lines(test$t, test$upr, col="red", lwd=1)
lines(test$t, test$lwr, col="red", lwd=1)

###