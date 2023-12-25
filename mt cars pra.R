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

#validation
M1p=predict(m1, newdata=Test)
r1=M1p-Test$mpg
MSE1=mean(r1^2)
RMSE1 = sqrt(MSE1)
MAE1 = mean(abs(r1))
MAPE1=mean(abs(r1/Test$mpg))

M2p=predict(m1a, newdata=Test)
r2=M2p-Test$mpg
MSE2=mean(r2^2)
RMSE2 = sqrt(MSE2)
MAE2 = mean(abs(r2))
MAPE2=mean(abs(r2/Test$mpg))

#m1a...


###outliers

#outlier y
influenceIndexPlot(m1a,id=list(n=3))

outlierTest(m1a) #reject Ho ,Fiat 128 is a outlier y
#outlier x  看leverage value (hat matix)
influencePlot(m1a,id=list(n=1))
p=length(m1a$coef)
n=nrow(mtcars)
limit1 = (2*p)/n #limit1 for Hat-Values of Bubble plot
limit2 = (3*p)/n #limit2 for Hat-Values of Bubble plot
#Lincoin Continenta為可疑的outlier x

#Check influential case using cook's distance
plot(cooks.distance(m1a))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m1a), row.names(mtcars))

#Check influential case using dfbetas
dfbetasPlots(m1a,id.n=2)

mtcars[rownames(mtcars)=='Fiat 128',]

newd <- mtcars[!rownames(mtcars)%in%c('Fiat 128'),]
m1c <- lm(mpg~cyl+wt,data=newd)
summary(m1c)

influenceIndexPlot(m1c,id=list(n=2))
n=nrow(newd)
plot(cooks.distance(m1c))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m1c), row.names(newd))
dfbetasPlots(m1c,id.n=2)