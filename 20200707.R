##
library("ggplot2")
library("tidyr")

##Dataset
urlfile <- "https://raw.githubusercontent.com/mosuzuki/IDSC_IDEpi/master/usflu.csv"

dat <- read_csv(url(urlfile))

dat$yrweek_dt <- as.Date(dat$yrweek_dt)

##Line plot
g1 <- ggplot(dat, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_line(size=0.8) +
  scale_x_date(date_labels=("%Y"), date_breaks="1 year", expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of Deaths from P&I") + 
  labs(title="Pneumonia and Influenza Mortality, USA") +
  theme_bw()

g1

##Bar plot
g2 <- ggplot(dat, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  scale_x_date(date_labels=("%Y"), date_breaks="1 year", expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of Deaths from P&I") + 
  labs(title="Pneumonia and Influenza Mortality, USA") +
  theme_bw()

g2

##Dataset
dat$t <- seq.int(nrow(dat))

dat1 <- subset(dat, fluyr<2014)

datnew <- subset(dat, fluyr==2014)

##Model fitting
fit <- lm(fludeaths ~ t + sinpi(2*t/52) + cospi(2*t/52), data = dat1)

##90% prediction interval
pred1 <- predict(fit, newdata = datnew, interval="predict", level=0.90)

datnew$fit1 <- pred1[,1]
datnew$lwr1 <- pred1[,2]
datnew$upr1 <- pred1[,3]

g3 <- ggplot(datnew, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  geom_line(aes(x=yrweek_dt, y=fit1), size=1, colour = "black") +
  geom_line(aes(x=yrweek_dt, y=upr1), size=1, colour = "blue") +
  geom_line(aes(x=yrweek_dt, y=lwr1), size=1, colour= "blue") +
  scale_x_date(date_labels=("%Y"), date_breaks="1 year", expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of Deaths from P&I") + 
  labs(title="Pneumonia and Influenza Mortality, USA") +
  theme_bw()

g3

##95% prediction interval
pred2 <- predict(fit, newdata = datnew, interval="predict", level=0.95)

datnew$fit2 <- pred2[,1]
datnew$lwr2 <- pred2[,2]
datnew$upr2 <- pred2[,3]

g4 <- ggplot(datnew, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  geom_line(aes(x=yrweek_dt, y=fit1), size=1, colour = "black") +
  geom_line(aes(x=yrweek_dt, y=upr1), size=1, colour = "blue") +
  geom_line(aes(x=yrweek_dt, y=upr2), size=1, colour= "red") +
  scale_x_date(date_labels=("%Y"), date_breaks="1 year", expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of Deaths from P&I") + 
  labs(title="Pneumonia and Influenza Mortality, USA") +
  theme_bw()

g4

##Excess mortality
datnew$excess <-  round(datnew$fludeaths - datnew$upr2)
datnew$excess[datnew$excess<0] <- 0
sum(datnew$excess)
