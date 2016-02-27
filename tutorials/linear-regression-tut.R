dataset = read.csv("data-marketing-budget-12mo.csv", header=T,
                   colClasses = c("numeric", "numeric", "numeric"))
head(dataset,5)
#/////Simple Regression/////
simple.fit = lm(Sales~Spend,data=dataset)
summary(simple.fit)

#Loading the necessary libraries
library(lmtest) #dwtest
library(fBasics) #JarqueBeraTest

#Testing normal distribution and independence assumptions
jarqueberaTest(simple.fit$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
dwtest(simple.fit) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated

#Simple Regression Residual Plots
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(simple.fit$resid~dataset$Spend[order(dataset$Spend)],
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(simple.fit$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(simple.fit$resid)
qqline(simple.fit$resid)

#///////////Multiple Regression Example///////////

multi.fit = lm(Sales~Spend+Month, data=dataset)
summary(multi.fit)

#Residual Analysis for Multiple Regression
dwtest(multi.fit) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
jarqueberaTest(multi.fit$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero

#Multiple Regression Residual Plots
layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted, rstudent(multi.fit),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions",ylab="Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(dataset$Month, multi.fit$resid,
     main="Residuals by Month",
     xlab="Month",ylab="Residuals")
abline(h=0,lty=2)
hist(multi.fit$resid,main="Histogram of Residuals")
qqnorm(multi.fit$resid)
qqline(multi.fit$resid)