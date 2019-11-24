#####################################################################
#20191126 
#Time series analysis: Time series regression models and seasonality"
#####################################################################

library("Epi")
library("forecast")
library("tsModel")

#US flu data
usflu <- read.csv("usflu.csv")
flu <- usflu

flu$d <- flu$alldeaths
flu$t <- seq.int(nrow(flu))

##########################################
#Generalized linear models (GLMs) for tend
##########################################

#Linear regression (log(y))
fit1 <- glm(d ~ t, data = flu, family=gaussian(link="log"))
pred1 <- predict(fit1, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred1$upr <- pred1$fit + 1.96 * pred1$se.fit
pred1$lwr <- pred1$fit - 1.96 * pred1$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Linear regression", ylab="All deaths")
lines(flu$t, pred1$fit, col="blue", lwd=3)
lines(flu$t, pred1$upr, col="blue", lwd=1)
lines(flu$t, pred1$lwr, col="blue", lwd=1)

#Poisson regression
fit2 <- glm(d ~ t, data = flu, family=poisson)
pred2 <- predict(fit2, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred2$upr <- pred2$fit + 1.96 * pred2$se.fit
pred2$lwr <- pred2$fit - 1.96 * pred2$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Poisson regression", ylab="All deaths")
lines(flu$t, pred2$fit, col="blue", lwd=3)
lines(flu$t, pred2$upr, col="blue", lwd=1)
lines(flu$t, pred2$lwr, col="blue", lwd=1)

#Quasi-poisson regression
fit3 <- glm(d ~ t, data = flu, family=quasipoisson)
pred3 <- predict(fit3, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred3$upr <- pred3$fit + 1.96 * pred3$se.fit
pred3$lwr <- pred3$fit - 1.96 * pred3$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Quasi-poisson regression", ylab="All deaths")
lines(flu$t, pred3$fit, col="blue", lwd=3)
lines(flu$t, pred3$upr, col="blue", lwd=1)
lines(flu$t, pred3$lwr, col="blue", lwd=1)

#Nagative binomial regression
library(MASS)
fit4 <- glm.nb(d ~ t, data = flu)
pred4 <- predict(fit4, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred4$upr <- pred4$fit + 1.96 * pred4$se.fit
pred4$lwr <- pred4$fit - 1.96 * pred4$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Negative binomial regression", ylab="All deaths")
lines(flu$t, pred4$fit, col="blue", lwd=3)
lines(flu$t, pred4$upr, col="blue", lwd=1)
lines(flu$t, pred4$lwr, col="blue", lwd=1)

###############################
#Polynomial regression for tend
###############################

#Quadratic polynomial regression (degree=2)
fit1.1 <- glm(d ~ t + I(t^2), data = flu, family=gaussian(link="log"))
pred1.1 <- predict(fit1.1, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred1.1$upr <- pred1.1$fit + 1.96 * pred1.1$se.fit
pred1.1$lwr <- pred1.1$fit - 1.96 * pred1.1$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Quadratic polynomial regression", ylab="All deaths")
lines(flu$t, pred1.1$fit, col="blue", lwd=3)
lines(flu$t, pred1.1$upr, col="blue", lwd=1)
lines(flu$t, pred1.1$lwr, col="blue", lwd=1)

#Cubic polynomial regression (degree=3)
fit1.2 <- glm(d ~ t + I(t^2) + I(t^3), data = flu, family=gaussian(link="log"))
pred1.2 <- predict(fit1.2, data = flu, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred1.2$upr <- pred1.2$fit + 1.96 * pred1.2$se.fit
pred1.2$lwr <- pred1.2$fit - 1.96 * pred1.2$se.fit

plot.ts(flu$t, flu$d, ylim=c(8000,16000), main = "Cubic polynomial regression", ylab="All deaths")
lines(flu$t, pred1.2$fit, col="blue", lwd=3)
lines(flu$t, pred1.2$upr, col="blue", lwd=1)
lines(flu$t, pred1.2$lwr, col="blue", lwd=1)

#####################
#Merge temperature
#####################

temp <- read.csv("temp.csv")

temp$date <- as.Date(temp$date)

flu$date <- as.Date(flu$yrweek_dt)

flu2 <- merge(flu, temp, by="date")
flu2$t <- seq.int(nrow(d))

layout(1:2)
plot(flu2$date, flu2$d, type="l", lwd=2, xlab="Year", ylab="All deaths")
plot(flu2$date, flu2$dtemp, type="l", lwd=2, col="red", xlab="Year", ylab="Temp")

par(mfrow=c(1,1))
####################################
#Effect of temperature on all deaths
####################################

#Unadjusted model
fit5.1 <- glm(d ~ dtemp, data = flu2, family=gaussian(link="log"))
round(ci.lin(fit5.1, subset="dtemp", Exp=T),3)
res5.1 <- residuals(fit5.1, type="response")
acf(res5.1)

#Adjusted for week
fit5.2 <- glm(d ~ dtemp + week, data = flu2, family=gaussian(link="log"))
round(ci.lin(fit5.2, subset="dtemp", Exp=T),3)
res5.2 <- residuals(fit5.2, type="response")
acf(res5.2)

#Adjusted for cubic spline, d.f.=6
library(splines)
spl = bs(flu2$t, degree=3, df=6)

fit5.3 <- glm(d ~ dtemp + spl, data = flu2, family=gaussian(link="log"))
round(ci.lin(fit5.3, subset="dtemp", Exp=T),3)
res5.3 <- residuals(fit5.3, type="response")
acf(res5.3)

#Adjusted for cubic spline, d.f.=24
spl2 = bs(flu2$t, degree=3, df=24)

fit5.4 <- glm(d ~ dtemp + spl2, data = flu2, family=gaussian(link="log"))
round(ci.lin(fit5.4, subset="dtemp", Exp=T),3)
res5.4 <- residuals(fit5.4, type="response")
acf(res5.4)

#Comparison of AIC
fit5.1$aic
fit5.2$aic
fit5.3$aic
fit5.4$aic

##################################
#Statistical tests for seasonality
##################################

#Kruskal-Wallis test
#By week
boxplot(d ~ week, data = flu2)
kruskal.test(d ~ week, data = flu2)

#By month
flu2$month <- format(flu2$date, "%m")
boxplot(d ~ month, data = flu2)
kruskal.test(d ~ month, data = flu2)

#Detrend by cubic polynomial fitted values
d1 <- flu2$d - fit1.2$fitted
plot.ts(flu2$t, d1, type="l")

boxplot(d1 ~ month, data = flu2)
kruskal.test(d1 ~ month, data = flu2)

#Friedman's test
flu3 <- subset(flu2, year>2010&year<2016&week<53)
table(flu3$year)
table(flu3$week)

friedman.test(d ~ week | year, data = flu3)

###
