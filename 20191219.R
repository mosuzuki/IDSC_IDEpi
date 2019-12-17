######################################
#Basic statistics and graphics using R
#Dec 19, 2019
######################################
#Basic math
2+3
5-3
3*4
6/2
1+2*3
(1+2)*3
1/2*3
1/(2*3)
2^3
25^0.5
sqrt(25)
abs(-5)

pi
round(1.2345, 2)

exp(2)
log(2)
log10(100)

#Assignment
x <- 1
x

2 -> x
x

x = 3
x

y <- 4

x+y
x^2+y/3

z<- "Hello"
z

##########################
#Data entry and management
##########################
#Vectors
age <- c(5, 11, 8, 15, 6)
age
age[2]

class(age)
length(age)

age[2] <- 9
age

age[2] <- NA
age

age <- age+1
age

gender <- c(1, 2, 1, 1, 2)
gender
class(gender)

gender <- as.factor(gender)
gender

levels(gender) <- c("male", "female")
gender
class(gender)

gender[is.na(age)] <- NA
gender

date <- c("2010-10-1", "2010-10-5", "2010-10-11", "2011-1-12", "2012-3-2")
date
class(date)

date <- as.Date(date)
date
class(date)

#Dataframe
df1 <- data.frame(age, gender, date)
df1

df1$age
class(df1)
View(df1)

df1$id <- c(1, 2, 3, 4, 5)
df1

df2 <- data.frame(score=c(69, 78, 91, 77, 88))
df2
df2$id <- seq.int(nrow(df2))
df2

#Merge two dataframes
df.new <- merge(df1, df2, by="id")
df.new
View(df.new)

#List
Taro <- list(age = 18, gender = "male",
               subjects = c("Math","English"))
Taro

#Working directory
getwd()
setwd("C:/Users/xxxxx")

#Save dataset
write.csv(df.new)

#Q1. Replace the missing value in age with 10.
#Q2. Generate a new vector x as age plus score.
#Q3. Overwrite the existing df.new file reflecting the above changes.

#################
#Basic statistics
#################
#Use "iris" dataset
help("iris")
head(iris)

attach(iris)

summary(iris)

#Summary measures for numerical variables
mean(Sepal.Length)
var(Sepal.Length)
sd(Sepal.Length)

median(Sepal.Length)
quantile(Sepal.Length, probs = c(.25,.75) )
IQR(Sepal.Length)

aggregate(Sepal.Length, list(Species), mean)

#Categorization
length.grp <- cut(Sepal.Length, breaks = c(0,5.8,Inf),
                  labels=c("<5.8","5.8-"))
length.grp

width.grp <- cut(Sepal.Width, breaks = c(0,3.1,Inf),
                  labels=c("<3.1","3.1-"))
width.grp

#Tabulation
table(Species)
table(length.grp)
tab <- table(Species, length.grp)

prop.table(tab)
prop.table(tab, margin=1)
prop.table(tab, margin=2)

###############
#Basic graphics
###############
#Histogram
hist(Sepal.Length)
hist(Sepal.Length, freq = F)
hist(Sepal.Length, freq = F, breaks = 20)

#Boxplot
boxplot(Sepal.Length, Sepal.Width)
boxplot(Sepal.Length~Species)

#Scatter plot
plot(Sepal.Length, Sepal.Width, pch=19, col="blue")

#Q. Draw a boxplot for demonstrating the distribution of Petal.Width by Species.

##################
#Statistical tests
##################
#Compatison of proportions
chisq.test(tab)
fisher.test(tab)

#t-test=comparison of 2 means
t.test(Sepal.Width~length.grp)
t.test(Sepal.Length, Sepal.Width)

#ANOVA=comparison of 3+ means
oneway.test(Sepal.Length~Species)

#Non-parametric tests
#Mann???Whitney U test=comparison of 2 groups
wilcox.test(Sepal.Length, Sepal.Width)
wilcox.test(Sepal.Width~length.grp)

#Kruskal-Wallis rank sum test=comparison of 3+ groups
kruskal.test(Sepal.Length~Species)

#Risk ratio and odds ratio
library(Epi)
tab2 <- table(length.grp, width.grp)
tab2
twoby2(tab2)

#Linear regression
plot(Sepal.Length, Sepal.Width, pch=19, col="skyblue")
mod <- lm(Sepal.Width~Sepal.Length)
pred <- predict(mod)
lines(pred, col="pink", lwd=2)

#Q. Conduct a statistical test for comparing Petal.Width by Species.

##################################
#Data visualization using ggplot2
##################################
library(ggplot2)

#Scatter plot
ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point()+
  theme_classic()

ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point(colour="red", size=3)+
  geom_smooth(method="lm")+
  theme_bw()

ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width, group=Species, colour=Species))+
  geom_point()+
  theme_bw()

ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width, group=Species, colour=Species))+
  geom_point()+
  geom_smooth(method="loess")+
  facet_wrap(~Species)

#Histogram
ggplot(iris,aes(x=Petal.Length))+
  geom_histogram()

ggplot(iris,aes(x=Petal.Length))+
  geom_histogram(binwidth = 0.1)

ggplot(iris,aes(x=Petal.Length, fill=Species))+
  geom_histogram(binwidth = 0.1)

#Line plot
y <- rnorm(1000, 0, 1)
t <- c(1:1000)
df <- data.frame(t,y)

ggplot(df,aes(x=t, y=y))+
  geom_line(colour="blue")+
  theme_bw()

#Bar plot
ggplot(iris,aes(x=Species,y=Petal.Length))+
  stat_summary(fun.y = "mean", geom="bar", colour="black", fill="grey")+
  geom_point(aes(colour=Species))+
  theme_light()

ggplot(iris,aes(x=Species,y=Petal.Length))+
  stat_summary(fun.y = "mean", fun.ymax = "max", fun.ymin = "min", colour="black")+
  theme_light()

#Violin plot
ggplot(iris,aes(x=Species,y=Petal.Length))+
  geom_violin(scale="count")+
  geom_point(aes(colour=Species))+
  theme_dark()

#Box plot
ggplot(iris,aes(x=Species,y=Petal.Width, fill=Species))+
  geom_boxplot()+
  theme_gray()

###############################
#Animated chart using gganimate
###############################
library(gganimate)
library(gapminder)

head(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.9) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")+
  theme_bw()
p

anim <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")

anim

#Check getwd() before save the gif file
anim_save("gapminder.gif", anim)

#####
