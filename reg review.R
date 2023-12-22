#Phase I: Identify the problem
#建立產品市場份額(Market Share)與其他影響因子間的迴歸關係,
#以了解其他影響因子與產品市場份額(Market Share)間的關係,
#並做為後續預測產品市場份額之用,
#以提供廠商參考

#Phase II: MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the important factors are included in the study.
#Depenent Varialbe: MShare
#Independent Variable: Other variables in the dataset.

#Phase III: Do the Math
setwd("C:\\Users\\tan90\\OneDrive\\大三\\迴歸\\week12")
Market=read.table("MarketShare.txt",header=T)
Market$DiscP <- as.factor(Market$DiscP)
Market$PProm <- as.factor(Market$PProm)

stem(Market$MShare)
stem(Market$Price)
stem(Market$GNrate)
plot(Market$DiscP)
table(Market$DiscP)
plot(Market$PProm)
table(Market$PProm)

set.seed(123)
Sindex=sample(nrow(Market),30)
Train=Market[Sindex,]
Test=Market[-Sindex,]

pairs(MShare~.,data=Train)
#cor(Train[,c(2,3,4,5,6,8)])

M1=lm(MShare~.,data=Train)  #建構初步模型
summary(M1)

##Phase IV(看四個假設):
library(car)
library(lmtest)
library(nortest)
library(randtests)
###Function Form and Homogeneity
#e=residuals(M1)
es=rstandard(M1)
residualPlot(M1,type="rstandard",quadratic=F)

resettest(M1,power=2,type='regressor')
ncvTest(M1)#This test is often called the Breusch-Pagan test; 

#Normality
qqPlot(M1)
lillie.test(es)#KS test for normality
shapiro.test(es)#Shapiro-Wilk Normality Test

plot(es,type = "l",col='2')
acf(es, ci=0.99)
#dwtest(M1)#Durbin-Watson test
runs.test(es)

#四個假設都過關