##############################
#R code for early alert system
#############################

#US flu dataset
usflu <- read.csv("usflu.csv")
flu <- usflu
flu$t <- seq.int(nrow(flu))

epi <- flu$epi
d <- flu$fludeaths
t <- seq.int(nrow(flu))

data <- data.frame(d,t,epi)

########################################################
#Basic Farrington's method using linear regression model
########################################################

fit <- lm(d ~ t^3 + t^2 + t + sinpi(2*t/52) + cospi(2*t/52), data = data)

#99% prediction interval
pred <- predict(fit, newdata = data, interval="predict", level=0.99)

data$fit <- pred[,1]
data$lwr <- pred[,2]
data$upr <- pred[,3]

plot(data$t, data$d, type="h", lwd=4, col="grey", xlim=c(104,311), 
     ylim=c(0,1400), main = "Basic Farrington", xlab="Year", ylab="Flu deaths", xaxt="n")
axis(1, at=0:5*52+12, labels=2011:2016)

lines(data$t, data$fit, col="black", lwd=1)
lines(data$t, data$upr, col="blue", lwd=2)
lines(data$t, data$lwr, col="black", lwd=1)

#Marking alert
data$alert <- data$d - data$upr
data$alert[data$alert<0] <- NA
data$alert[data$alert>0] <- 0
points(data$t, data$alert, pch=2, col="red")

##########################################################
#Flexible Farrington's method using "surveillance" package
##########################################################
library(surveillance)

observed <- data$d
state <- data$t

data2 <- data.frame(state, observed)

flu_new <- sts(data2$observed, frequency = 52, start=c(2010,42))

ctrl <-  list(range=NULL,
                  noPeriods=1,populationOffset=FALSE,
                  fitFun="algo.farrington.fitGLM.flexible",
                  b=2,w=2,weightsThreshold=1,
                  pastWeeksNotIncluded=2,
                  pThresholdTrend=0.1,trend=TRUE,
                  thresholdMethod="delta",alpha=0.01)

far <- farringtonFlexible(flu_new, control=ctrl)

#Plot
y.max <- max(upperbound(far),observed(far),na.rm=TRUE)

plot(far, ylim=c(0, y.max), main="Flexible Farrington", xlab="Year", ylab="No. of flu deaths", legend.opts=NULL)
lines(1:(nrow(far)+1)-0.5, 
      c(upperbound(far),upperbound(far)[nrow(far)]),
      type="s",col="blue",lwd=2)

###########################
#Hidden Markov model
###########################
library(depmixS4)

set.seed(123)
mod <- depmix(d~t^3 + t^2 + t + sinpi(2*t/52) + cospi(2*t/52), data=data, nstates=2, family=gaussian())
fit <- fit(mod,verbose = FALSE)
posterior(fit)

#Marking alert
cutoff <- 0.99
data$alert2 <- posterior(fit)[,3]

data$alert2[data$alert2<cutoff] <- NA
data$alert2[data$alert2>=cutoff] <- 0

#Plot
layout(1:2)
plot(data$t, data$d, type="h", lwd=5, col="grey", xlim=c(104,311), 
     ylim=c(0,1400), main = "Hidden Markov Model", xlab="Year", ylab="Flu deaths", xaxt="n")
axis(1, at=0:5*52+12, labels=2011:2016)
points(data$t, data$alert2, pch=2, col="red")

matplot(posterior(fit)[,-1], type='l', xlim=c(104,311), main="Probability of outbreak", ylab='Probability of outbreak')

###
