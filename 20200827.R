library("dplyr")
library("RCurl")
library("ggplot2")

#####################################
###MHLW daily pcr positive dataset###
#####################################

case <- getURL("https://www.mhlw.go.jp/content/pcr_positive_daily.csv")

caseInc <- read.csv(text=case)

colnames(caseInc) <- c("date", "case")

caseInc$date <- as.Date(caseInc$date)

##############################
###MHLW daily death dataset###
##############################

death <- getURL("https://www.mhlw.go.jp/content/death_total.csv")

deathInc <- read.csv(text=death)

colnames(deathInc) <- c("date", "cumdeath")

deathInc$date <- as.Date(deathInc$date)

########################
###Merge two datasets###
########################

dfInc <- merge(caseInc, deathInc, by="date", all=T)

dfInc[is.na(dfInc)] <- 0

dfInc$cumcase <- cumsum(dfInc$case)

###Plot###
g1 <- ggplot(dfInc, aes(x=date))+
  geom_line(stat="identity", aes(y=cumcase), colour="blue", lwd=1.2)+
  xlab("Date")+
  ylab("Cum cases")+
  scale_x_date(breaks = as.Date(c("2020-02-01", "2020-04-01", 
                                  "2020-06-01", "2020-08-01")))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 0),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

g1

g2 <- ggplot(dfInc, aes(x=date))+
  geom_line(stat="identity", aes(y=cumdeath), colour="tomato", lwd=1.2)+
  xlab("Date")+
  ylab("Cum deaths")+
  scale_x_date(breaks = as.Date(c("2020-02-01", "2020-04-01", 
                                  "2020-06-01", "2020-08-01")))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 0),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

g2

####################################
###To estimate population at-risk###
####################################

dfInc$t <- seq.int(nrow(dfInc))

###Distribution of the delay from report to death###
shape <- 1.42991231
rate <- 0.09796399

x <- c(0:100)
plot(x,dgamma(x,shape,rate),type="l")

###Cumulative population at-risk###
N <- nrow(dfInc)

result.d0 <- as.data.frame(dfInc$case*pgamma(1, shape, rate))
colnames(result.d0) <- c("X0")
result.d0$t <- seq.int(nrow(result.d0))

result.lag <- data.frame(matrix(nrow = N, ncol = N -1))
result.lag$t <- seq.int(nrow(result.lag))

for (i in 1:(N-1) ){ 
  cum <- lag(dfInc$case, i)*pgamma(i+1, shape, rate)
  result.lag[i] <- cum
}

result.all <- merge(result.d0, result.lag, by="t", all=T)
result.all[is.na(result.all)] <- 0
result.all$atrisk <- rowSums(result.all)-result.all$t

dfAtrisk <- data.frame(result.all$t, result.all$atrisk)
colnames(dfAtrisk) <- c("t","atrisk")

dfInc <- merge(dfInc,dfAtrisk, by="t", all=T)

############################
###Estimation of the CFRs###
############################

###Crude CFR###
dfInc$crude.cfr <- dfInc$cumdeath/dfInc$cumcase

###14-day lagged CFR###
dfInc <- dfInc %>%
  mutate(lag = lag(dfInc$cumcase,n = 14))

dfInc$lag.cfr <- dfInc$cumdeath/dfInc$lag

###Adjusted CFR###
dfInc$adj.cfr <- dfInc$cumdeath/dfInc$atrisk
dfInc$adj.cfr.lwr <- dfInc$adj.cfr-1.96*sqrt((dfInc$adj.cfr*(1-dfInc$adj.cfr))/(dfInc$cumdeath*(1/dfInc$adj.cfr)))
dfInc$adj.cfr.upr <- dfInc$adj.cfr+1.96*sqrt((dfInc$adj.cfr*(1-dfInc$adj.cfr))/(dfInc$cumdeath*(1/dfInc$adj.cfr)))

###Plot###
g3 <- ggplot(data=subset(dfInc, dfInc$date>as.Date("2020-02-14")), aes(x=date))+
  geom_line(stat="identity", aes(y=adj.cfr, colour="tomato"), lwd=1.2)+
  geom_ribbon(aes(ymin=adj.cfr.lwr, ymax=adj.cfr.upr), fill="tomato", alpha=0.3)+
  geom_line(stat="identity", aes(y=crude.cfr), colour="blue", lwd=1.2)+
  geom_line(stat="identity", aes(y=lag.cfr), colour="black", lwd=1.2)+
  xlab("Date")+
  ylab("CFR")+
  ylim(0,0.15)+
  scale_x_date(breaks = as.Date(c("2020-03-01", "2020-04-01", 
                                  "2020-05-01", "2020-06-01",
                                  "2020-07-01", "2020-08-01")))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 0),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "none")

g3

############################
###Mean CFR by the period###
############################

mean(dfInc$adj.cfr[dfInc$date>="2020-05-01"&dfInc$date<="2020-05-31"], na.rm=T)
mean(dfInc$adj.cfr.lwr[dfInc$date>="2020-05-01"&dfInc$date<="2020-05-31"], na.rm=T)
mean(dfInc$adj.cfr.upr[dfInc$date>="2020-05-01"&dfInc$date<="2020-05-31"], na.rm=T)

mean(dfInc$adj.cfr[dfInc$date>="2020-07-01"&dfInc$date<="2020-07-31"], na.rm=T)
mean(dfInc$adj.cfr.lwr[dfInc$date>="2020-07-01"&dfInc$date<="2020-07-31"], na.rm=T)
mean(dfInc$adj.cfr.upr[dfInc$date>="2020-07-01"&dfInc$date<="2020-07-31"], na.rm=T)

###
###
