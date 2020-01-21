library("MASS")
library("fitdistrplus")
library("ggplot2")

#Natsuko Imai, et al
#https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/news--wuhan-coronavirus/

p <- (3301/19000000)*10
x <- 3

#exact
x/p

#simulated
N <- (rnbinom(n = 10000, size = x, prob = p)) + x
hist(N)

fit=fitdist(N, "nbinom", method="mle")

summary(fit)

fit$estimate

size <- fit$estimate[1]
mu <- fit$estimate[2]
size
mu

y=c(0:10000)

fit <- dnbinom(x=y, size=size, mu=mu)
plot(fit, type="l")

qnbinom(0.025, size=size, mu=mu)
qnbinom(0.975, size=size, mu=mu)

#####

#install.packages("EpiModel", dependencies = TRUE)
library("EpiModel")

param <- param.dcm(inf.prob = 0.2, act.rate = 0.25)
init <- init.dcm(s.num = 19000000, i.num = 1)
control <- control.dcm(type = "SI", nsteps = 100)


mod <- dcm(param, init, control)
mod
plot(mod)

summary(mod, at = 15)

###

SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and FOI from a rearrangement of Beta * c * D
    ce <- R0 / i.dur
    lambda <- ce * i.num/num
    
    dS <- -lambda*s.num
    dE <- lambda*s.num - (1/e.dur)*e.num
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num,
         ei.prev = (e.num + i.num)/num)
  })
}


param <- param.dcm(R0 = c(0.9, 1.2, 3), e.dur = 3, i.dur = 5, cfr = 0.01)
init <- init.dcm(s.num = 19000000, e.num = 1, i.num = 0, r.num = 0,
                 se.flow = 0, ei.flow = 0, ir.flow = 0, d.flow = 0)
control <- control.dcm(nsteps = 50, dt = 1, new.mod = SEIR)
mod <- dcm(param, init, control)

mod1 <- as.data.frame(mod)
mod1$cum.num <- 19000000-mod1$s.num

par(mfrow = c(1, 2, 3))
ggplot(mod1, aes(x=time, y=cum.num))+
  geom_line(colour="blue", size=1.2)+
  ylim(0,2000)+
  geom_line(y=c(200), colour="black", linetype="dashed")+
  geom_line(y=c(1700), colour="black", linetype="dashed")+
  theme_bw()+
  facet_wrap(~run)

#####
