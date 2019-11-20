#US flu dataset
usflu <- read.csv("usflu.csv")
flu <- usflu
flu$t <- seq.int(nrow(flu))

#Linear regression with one Fourier term
fit <- lm(fludeaths ~ t + sinpi(2*t/52) + cospi(2*t/52), data = flu)

#90% prediction interval
pred <- predict(fit, newdata = flu, interval="predict", level=0.90)

flu$fit <- pred[,1]
flu$lwr <- pred[,2]
flu$upr <- pred[,3]

plot(flu$t, flu$fludeaths, type="h", lwd=2, col="grey", xlim=c(104,311), ylim=c(0,1400), main = "Weekly flu deaths in US", xlab="Year", ylab="Flu deaths", xaxt="n")
axis(1, at=0:5*52+12, labels=2011:2016)

lines(flu$t, flu$fit, col="black", lwd=1)
lines(flu$t, flu$upr, col="blue", lwd=2)
lines(flu$t, flu$lwr, col="black", lwd=2)

#Farrington's method
library(surveillance)

observed <- flu$fludeaths
state <- flu$t

flu <- data.frame(state, observed)

flu_new <- sts(flu$observed, frequency = 52, start=c(2010,42))

ctrl <-  list(range=NULL,
                  noPeriods=1,populationOffset=FALSE,
                  fitFun="algo.farrington.fitGLM.flexible",
                  b=2,w=2,weightsThreshold=1,
                  pastWeeksNotIncluded=2,
                  pThresholdTrend=0.1,trend=TRUE,
                  thresholdMethod="delta",alpha=0.1)

far <- farringtonFlexible(flu_new, control=ctrl)

#Plot
y.max <- max(upperbound(far),observed(far),na.rm=TRUE)

plot(far, ylim=c(0, y.max), main='Weekly Flu Deaths in US', xlab="Year", ylab="No. of flu deaths", legend.opts=NULL)
lines(1:(nrow(far)+1)-0.5, 
      c(upperbound(far),upperbound(far)[nrow(far)]),
      type="s",col="blue",lwd=2)
