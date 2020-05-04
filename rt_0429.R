library("incidence")
library("ggplot2")
library("R0")
library("earlyR")
library("mice")


setwd("C:/Users/xxxxx/")
cov <- read.csv("cov0504.csv")

cov$ons <- as.Date(cov$ons,format="%m/%d/%y", origin="1970-01-01")
cov$rep <- as.Date(cov$rep,format="%m/%d/%y", origin="1970-01-01")

cov$del <- as.numeric(cov$rep-cov$ons)

#simple imputation for date of onset
cov.1 <- data.frame(cov$seq, cov$pref, cov$age, cov$sex, cov$import, cov$symp, cov$contact, cov$del)

cov.1$cov.age[is.na(cov.1$cov.age)] <- 999
cov.1$cov.import[is.na(cov.1$cov.import)] <- 999
cov.1$cov.symp[is.na(cov.1$cov.symp)] <- 999
cov.1$cov.contact[is.na(cov.1$cov.contact)] <- 999


imp <- mice(cov.1, method = "norm.predict", m = 1)

cov.imp <- complete(imp)
cov.imp$cov.del[cov.imp$cov.del<0] <-0
cov.imp$cov.del <- round(cov.imp$cov.del)

cov.2 <- data.frame(cov.imp$cov.seq, cov.imp$cov.del)
cov.2$seq <- cov.2$cov.imp.cov.seq
cov.2$del <- cov.2$cov.imp.cov.del

cov.3 <- merge(cov, cov.2, by = "seq", all = T) 
cov.3$ons <- cov.3$rep - cov.3$del.y 

cov <- cov.3

#estimated date of infection
n <- nrow(cov)
lamda <- 5
cov$inc <- rpois(n, lamda)

cov$inf <- cov$ons-cov$inc

cov$import[cov$import>2] <- 0

###
#Tokyo and surrounding prefectures
#cov <- subset(cov, pref=="東京"|pref=="神奈川"|pref=="埼玉"|pref=="千葉")
#cov <- subset(cov, pref=="北海道")
###

cov.imp <- subset(cov, cov$import==1)

#daily incidence

inf <- incidence(cov$inf, interval = 1)
imp <- incidence(cov.imp$inf, interval = 1)

inc.inf <- data.frame(inf$dates, inf$counts)

inc.imp <- data.frame(imp$dates, imp$counts)
colnames(inc.imp) <- c("inf.dates","imp.counts")

df.1 <- merge(inc.inf, inc.imp, by = "inf.dates", all = T) 
df.1$imp.counts[is.na(df.1$imp.counts)] <- 0

#effective reproduction number
#Bi et al. LID 2020.
GT <- generation.time("weibull", c(6.3, 4.2))
#Nishiura
#GT <- generation.time("weibull", c(4.2, 2.9))

TD <- est.R0.TD(epid=df.1$inf.counts, GT, import = df.1$imp.counts, begin=18, end=114, nsim=2000)

df <- data.frame(TD$R, TD$conf.int)

df$inf.dates <- seq(as.Date("2020-01-20"), as.Date("2020-04-25"), "days")

df <- merge(inc.inf, df, by = "inf.dates", all = T) 

ggplot(df, aes(x=inf.dates, y=inf.counts))+
  xlab("Estimated date of infection")+
  ylab("No. counts")+
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  geom_line(y=df$TD.R*100, colour="red", lwd=1.2)+
  geom_ribbon(aes(ymin=df$lower*100,ymax=df$upper*100, fill="orange"), alpha=0.5)+
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-01-15",
                                  "2020-02-01", "2020-02-15",
                                  "2020-03-01", "2020-03-15",
                                  "2020-04-01", "2020-04-15"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/100), name = "Effective reproduction number"))+
  geom_line(y=100, colour="black", linetype = "dashed")+
  labs(title="Entire Japan: by M.Suzuki")+
  theme(legend.position ="none")

###
