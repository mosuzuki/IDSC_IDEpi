##############################
#20191203
#Vaccine impact studies
##############################
dat1203 <- read.csv("dat1203.csv")
dat <- dat1203

summary(dat)

##########################
#Rota vaccine impact study
##########################

#Dataset for pre-vaccine period

dat0 <- subset(dat, year<2014)

plot(dat$time, dat$d, type="l",lwd=3, col="blue",ylim=c(0,2500))

mod1 <- glm(d~offset(pop)+time+year+factor(month),data=dat0,family=quasipoisson)

pred <- predict(mod1, newdata=dat, type="response", se.fit=TRUE, interval="confidence", level=0.95)

pred$upr <- pred$fit + 1.96 * pred$se.fit
pred$lwr <- pred$fit - 1.96 * pred$se.fit

lines(dat$t, pred$fit,col="red",lwd=3)
lines(dat$t, pred$upr,col="red",lwd=1)
lines(dat$t, pred$lwr,col="red",lwd=1)

#Impact of the vaccination policy

mod2 <- glm(d ~ offset(log(pop)) + time + year + factor(month) + vac, data=dat, family=quasipoisson)

summary(mod1)

library("Epi")
round(ci.lin(mod2,Exp=T), 2)

#GE hospitalization averted by vaccine

dat1 <- subset(dat, year>=2014)

dat1$pred <- predict(mod1, newdata=dat1, type="response")

dat1$no_avert <- dat1$pred-dat1$d

dat1$vac_pop <- dat1$pop*dat1$prop_vac

total_num <- formatC(sum(dat1$no_avert))

total_denom <- formatC(sum(dat1$vac_pop))

as.numeric(total_num)*12*1000/as.numeric(total_denom)


###############################
#Varicella vaccine impact study
###############################
plot(dat$time, dat$d2, pch=19, ylim=c(0,1200))

#Linear regression
#Same slope
mod3.1 <- glm(d2~time+factor(vac2), data=dat, family=gaussian())
round(ci.lin(mod3.1,Exp=T), 3)

dat$pred3.1 <- predict(mod3.1, data=dat, type="response")
lines(dat$time, dat$pred3.1, col="red", lwd=3)

#Separate slopes
mod3.2 <- glm(d2 ~ time + factor(vac2) + time:factor(vac2), data=dat, family=gaussian())
round(ci.lin(mod3.2,Exp=T), 3)

dat$pred3.2 <- predict(mod3.2, data=dat, type="response")
lines(dat$time, dat$pred3.2, col="blue", lwd=3)

#Nagative binomial regression
library(MASS)
#Same slope
mod3.3 <- glm.nb(d2 ~ time + factor(vac2), data=dat)
round(ci.lin(mod3.3,Exp=T), 2)

dat$pred3.3 <- predict(mod3.3, data=dat, type="response")

plot(dat$time, dat$d2, pch=19, ylim=c(0,1200))
lines(dat$time, dat$pred3.3, col="red", lwd=3)

#Separate slopes
mod3.4 <- glm.nb(d2 ~ time + factor(vac2) + time:factor(vac2), data=dat)
round(ci.lin(mod3.4,Exp=T), 2)

dat$pred3.4 <- predict(mod3.4, data=dat, type="response")

lines(dat$time, dat$pred3.4, col="blue", lwd=3)


######
plot(dat$time, dat$d2, pch=19, ylim=c(0,1200))

dat$y1 <- 124.333*(1.03^dat$time)
dat$y2 <- (124.333*1.644)*((1.03*0.994)^dat$time)
dat$y3 <- (124.333*3.289)*((1.03*0.986)^dat$time)

lines(dat$time, dat$y1, col="red", lwd=3)
lines(dat$time, dat$y2, col="blue", lwd=3)
lines(dat$time, dat$y3, col="green", lwd=3)

#####