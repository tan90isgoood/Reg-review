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

N <- nrow(mtcars)
set.seed(234)
ind <- sample(N,round(0.8*N))
Train <- mtcars[ind,]
Test <- mtcars[-ind,]
m1 <- lm(mpg~.,data=Train)
summary(m1)
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

###phase V: (簡化模型 因為四個假設都過關 所以不需要修改)
s1=step(m1)
s2=step(m1,k=log(nrow(Train))) #BIC

m1a <- lm(mpg~cyl+wt,data=Train)
summary(m1a)

vif(m1)
vif(m1a)