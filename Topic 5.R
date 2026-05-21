rm(list = ls())
y=c(16.68,11.5,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,21,13.5,19.75,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
x1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,10,6,7,3,17,10,26,9,8,4)
x2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,776,200,132,36,770,140,810,450,635,150)
data_soft=data.frame(y,x1,x2)
pairs(data_soft)
cor(data_soft)
fit=lm(y~x1+x2,data=data_soft)
fit
summary(fit)
anova(fit)
#b)
e=fit$residuals
qqnorm(e)
qqline(e)
n=length(y)
x=matrix(c(rep(1,n),x1,x2),nrow = n,byrow=F)
x
H=x%*%solve((t(x)%*%x))%*%t(x)
H
h=diag(H)
h

#direct command for design matrix
x1=model.matrix(fit)

#direct command for hat matrix
hatvalues(fit)
Y=matrix(y,nrow = n)
beta_hat=solve(t(x)%*%x)%*%t(x)%*%Y
beta_hat
y_hat=x%*%beta_hat
SSE=t(Y-y_hat)%*%(Y-y_hat)
SSE
p=3
MSE=SSE/(n-p)
MSE
e=y-y_hat

#direct command for MSE
MSE=anova(fit)[3,3]
MSE
std_res=e/sqrt(MSE*(1-h))
std_res

#direct command for studantized residuals
rstandard(fit)
qqnorm(std_res)
qqline(std_res)

#c)
plot(y_hat,e)
#d)
plot(seq(1,n),e)
#e)
shapiro.test(fit$residuals)
library(lmtest)
bptest(fit)
dwtest(fit)
 #f)
# TO check the outlier in  data
d=e/sqrt(MSE)
d
which(abs(d)>2)
rstandard(fit)
press=e/(1-h)
press
r=rstudent(fit)
r
r[abs(r)>2]
#g)

y1=y[-9]
X1=x1[-9]
X2=x2[-9]
df1=data.frame(y1,X1,X2)
df1
model=lm(y1~X1+X2,data=df1)
summary(model)
anova(model)

#Q2
y=c(26,24,175,160,163,55,62,100,26,30,70,71)
x1=c(1,1,1.5,1.5,1.5,1,2,0.5,1,1,1,1)
x2=c(1,1,4,4,4,2,5,3,2,2,3,3)
df=data.frame(y,x1,x2)
fit1=lm(y~x1+x2,data=df)
fit2=lm(y~0+as.factor(x1)+as.factor(x2),data=df)
anova(fit1,fit2)
