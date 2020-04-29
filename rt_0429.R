library("incidence")
library("ggplot2")
library("R0")
library("earlyR")

setwd("C:/xxxxx")
cov <- read.csv("cov0428.csv")

#Tokyo and surrounding prefectures
#cov <- subset(cov, pref=="“Œ‹ž"|pref=="_“Þì"|pref=="é‹Ê"|pref=="ç—t")

cov$ons <- as.Date(cov$ons,format="%m/%d/%y")
cov$rep <- as.Date(cov$rep,format="%m/%d/%y")

#simple imputation for date of onset as date of report - 7ds
cov$ons[is.na(cov$ons)] <- cov$rep-7

#estimated date of infection as date of onset - 5ds
cov$inf <- cov$ons-5

cov$import[cov$import>2] <- 0
cov.imp <- subset(cov, cov$import==1)

rep <- incidence(cov$rep, interval = 1)
ons <- incidence(cov$ons, interval = 1)
inf <- incidence(cov$inf, interval = 1)
imp <- incidence(cov.imp$inf, interval = 1)

inc.inf <- data.frame(inf$dates, inf$counts)

inc.imp <- data.frame(imp$dates, imp$counts)
colnames(inc.imp) <- c("inf.dates","imp.counts")

df.1 <- merge(inc.inf, inc.imp, by = "inf.dates", all = T) 
df.1$imp.counts[is.na(df.1$imp.counts)] <- 0

###
#EID
#GT <- generation.time(type="weibull", c(3.96,4.75))
#Nishiura
GT <- generation.time("weibull", c(4.2, 2.9))

TD <- est.R0.TD(epid=df.1$inf.counts, GT, import = df.1$imp.counts, begin=18, end=110, nsim=2000)

df <- data.frame(TD$R, TD$conf.int)

df$inf.dates <- seq(as.Date("2020-01-20"), as.Date("2020-04-21"), "days")

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
  theme(legend.position ="none")

####
