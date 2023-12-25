###Phase I: Identify the problem
#Hocking [original transcriber]'s noncrucial 
#coding of the Mazda's rotary engine as a straight six-cylinder engine 
#and the Porsche's flat engine as a V engine, as well as 
#the inclusion of the diesel Mercedes 240D, have been retained 
#to enable direct comparisons to be made with previous analyses.’

###phase II:MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the important factors are included in the study.
#Depenent Varialbe: mpg
#Independent Variable: Other variables in the dataset.

###phase III: do the math
data(mtcars)
View(mtcars)

mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)
pairs(mtcars)
m1 <- lm(mpg~.,data=mtcars)
summary(m1)

N <- nrow(mtcars)
set.seed(234)
ind <- sample(N,round(0.8*N))
Train <- mtcars[ind,]
Test <- mtcars[-ind,]

##Phase IV(看四個假設):
library(car)
library(lmtest)
library(nortest)
library(randtests)
#function form
es=rstandard(m1)
residualPlot(m1,type="rstandard",quadratic=F)
resettest(m1,power=2,type='regressor')
#homogeneity
ncvTest(m1)
#independency
plot(es,type = "l",col='2')
acf(es, ci=0.99)
runs.test(es)
#Normality
qqPlot(m1)
lillie.test(es)#KS test for normality
shapiro.test(es)
#都沒問題

