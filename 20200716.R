###Packages###
library("R0")
library("ggplot2")
library("EpiEstim")

###Dataset###
urlfile <- "https://raw.githubusercontent.com/mosuzuki/IDSC_IDEpi/master/cov_0716.csv"

cov <- read.csv(url(urlfile))

cov$date <- as.Date(cov$date,format="%m/%d/%y", origin="1970-01-01")

cov <- subset(cov, cov$date<"2020-07-16")

###Generation time###
#Bi et al. LID 2020.
#GT <- generation.time("weibull", c(6.3, 4.2))
#Nishiura et al. IJID 2020.
GT <- generation.time("weibull", c(4.7, 2.9))

###Basic reproduction number###
###Estimate R0 using maximum likelihood###
est.R0.ML(epid=cov$case, GT, begin=1, end=76)

est.R0.ML(epid=cov$case, GT, begin=28, end=76)

###Estimate R0 using exponential growth rate###
est.R0.EG(epid=cov$case, GT, begin=28, end=76)

###Using both methods###
R0 <- estimate.R(epid=cov$case, GT, begin=28, end=76, methods=c("EG", "ML"))

R0

###Time-dependent R###
###Using R0 package###
TD <- est.R0.TD(epid=cov$case, GT, begin=30, end=181)

TD

plot(TD)

df <- data.frame(TD$R, TD$conf.int)

df$date <- seq(as.Date("2020-02-14"), as.Date("2020-07-14"), "days")

df <- merge(cov, df, by = "date", all = T) 

g1 <- ggplot(df, aes(x=date, y=case))+
  xlab("•ñ“ú")+
  ylab("Ç—á”")+
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  geom_line(y=df$TD.R*200, colour="red", lwd=1.2)+
  geom_ribbon(aes(ymin=df$lower*200,ymax=df$upper*200, fill="orange"), alpha=0.5)+
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-01-15",
                                  "2020-02-01", "2020-02-15",
                                  "2020-03-01", "2020-03-15",
                                  "2020-04-01", "2020-04-15",
                                  "2020-05-01", "2020-05-15",
                                  "2020-06-01", "2020-06-15",
                                  "2020-07-01", "2020-07-15"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/200), name = "ŽÀŒøÄ¶ŽY”"))+
  geom_line(y=200, colour="black", linetype = "dashed")+
  labs(title="“ú–{‘S‘‚ÌV‹K•ñ”‚ÆŽÀŒøÄ¶ŽY”i•ñ“ú•Êj")+
  theme(legend.position ="none")

g1

###Using EpiEstim package###

res <- estimate_R(incid = cov$case, method = "parametric_si",
                  config = make_config(list(
                    mean_si = 4.7, std_si = 2.9)))

r.m <- res$R$`Mean(R)`
r.l <- res$R$`Quantile.0.025(R)`
r.u <- res$R$`Quantile.0.975(R)`

cov$t <- seq.int(nrow(cov))

df.1 <- data.frame(r.m, r.l, r.u)
df.1$t <- seq.int(nrow(df.1))+7

df.2 <- merge(cov, df.1, by = "t", all = T) 

d <- "2020-02-20"

df.2$r.m[df.2$date<d] <- NA
df.2$r.u[df.2$date<d] <- NA
df.2$r.l[df.2$date<d] <- NA

g2 <- ggplot(df.2, aes(x=date, y=case))+
  xlab("•ñ“ú")+
  ylab("Ç—á”")+
  geom_bar(stat = "identity", colour="black", fill="lightblue")+
  geom_line(y=df.2$r.m*200, colour="red", lwd=1.2)+
  geom_ribbon(aes(ymin=df.2$r.l*200,ymax=df.2$r.u*200, fill="orange"), alpha=0.5)+
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-01-15",
                                  "2020-02-01", "2020-02-15",
                                  "2020-03-01", "2020-03-15",
                                  "2020-04-01", "2020-04-15",
                                  "2020-05-01", "2020-05-15",
                                  "2020-06-01", "2020-06-15",
                                  "2020-07-01", "2020-07-15"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/200), name = "ŽÀŒøÄ¶ŽY”"))+
  geom_line(y=200, colour="black", linetype = "dashed")+
  labs(title="“ú–{‘S‘‚ÌV‹K•ñ”‚ÆŽÀŒøÄ¶ŽY”i•ñ“ú•Êj")+
  theme(legend.position ="none")

g2

