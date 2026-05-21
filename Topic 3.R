x=c(2,7,9,4,12,6,9,3,3,2)
y=c(650,1200,1300,430,1400,900,1800,640,793,925)
# i)
plot(x,y,xlab=)
cor(x,y)
cor.test(x,y,method="pearson")
fit=lm(y~x)
fit
n=length(y)
data=matrix(c(rep(1,n),x),nrow=n,byrow=F)
data
Y=matrix(y,nrow=n)
r=qr(data)$rank
r
s=t(data)%*%data
beta_hat=solve(s)%*%t(data)%*%Y
beta_hat
summary(fit)
qf(0.05,1,8,lower.tail=F)
qt(0.05,8,lower.tail = F)
abline(fit)
anova(fit)
# To find  95% confidence interval for B0 and B1
confint(fit,level=0.95)
# 
predict(fit,interval="confidence",level=0.95)

# vii)
452.66+96.69 *5
# OR
predict(fit,data.frame(x=5))

# viii)
res=fit$residuals
res
shapiro.test(res)
qqnorm(res)
qqline(res)
par(mfrow=c(2,2))
plot(fit)
library(lmtest)
bptest(fit)
par(mfrow = c(2,2))
plot(fit)
shapiro.test(residuals(fit))



#Q2
library(ISLR2)
data(Auto)
View(Auto)
data(Auto)
model=lm(mpg ~ horsepower,data=Auto)
model
summary(model)
data(Auto)
cor.test(Auto$mpg,Auto$horsepower,method="pearson")
plot(Auto$mpg,Auto$horsepower)
abline(Auto$mpg,Auto$horsepower)
par(mfrow=c(2,2))
plot(model)
res=model$residuals
shapiro.test(res)
library(lmtest)
bptest(res)
fit2=lm(mpg ~ horsepower +I(horsepower^2),data=Auto)
summary(fit2)
plot(fit2)
bptest(fit2)

