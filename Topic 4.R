#Q1
library(ISLR2)
data(Auto)
#View(Auto)
pairs(Auto)
cor(Auto[,-c(8,9)])
model=lm(mpg~ . -name -origin,data=Auto)
model
summary(model)
anova(model)
confint(model,level=0.95)
par(mfrow=c(2,2))
plot(model)

#Q2
Age=c(22,19,23,31,21,41,27,26,18,34,31,28,24,20,21,19,35)
Height=c(175,152,165,162,193,137,182,162,172,160,172,160,163,185,190,168,137)
DBP=c(60,70,82,90,68,76,76,62,74,76,70,80,62,78,76,68,60)
SBP=c(122,102,118,108,120,104,120,116,118,102,112,122,118,124,120,102,106)
Volume=c(3.1,3.4,3,3.2,4.9,2.4,4.5,3.1,4.4,2.9,4.2,3,3,4.7,4.8,4.1,2.3)
df=data.frame(Age,Height,DBP,SBP,Volume)
data
pairs(data)
cor(data)
model1=lm(Volume~.,data=df)
model1
summary(model1)
f_tab=qf(0.05,4,12,lower.tail = F)
f_tab
res=model1$residuals
y_hat=model1$fitted.values
cbind(Volume,y_hat,res)
confint(model1,level=0.95)
#c)
# 95% CI for mean response (E[Y|X])
predict(model1,interval = "confidence")
predict(model1,data.frame(Age=24,Height=164,DBP=75,SBP=115))
#d)
predict(model1,data.frame(Age=24,Height=164,DBP=75,SBP=115),interval = "confidence")
predict(model1,interval = "prediction")

#e)
fit1=lm(Volume~.,data=df)
anova(fit1)
SSE_om=anova(fit1)[5,2]
MSE_om=anova(fit1)[5,3]
fit2=lm(Volume~Age+Height,data=df)
SSE_rm=anova(fit2)[3,2] 
SSH0=SSE_rm-SSE_om
r=2
F_0=(SSH0/r)/MSE_om
F_0
F_tab=qf(0.05,r,12,lower.tail=F)
f_tab
ifelse(F_0>F_tab,"reject H0","Fails to reject H0")
#direct command
anova(fit2,fit1)
###
fit1=lm(Volume~.,data=df)
SSE_om=anova(fit1)[5,2]
MSE_om=anova(fit1)[5,3]
fit3=lm(Volume~Age+SBP,data=df)
anova(fit3)
SSE_rm=anova(fit3)[3,2]
SSH0=SSE_rm-SSE_om
r=2
F_0=(SSH0/r)/MSE_om
F_0
F_tab=qf(0.05,r,12,lower.tail=F)
f_tab
ifelse(F_0>F_tab,"reject H0","Fails to reject H0")

anova(fit3,fit1)

#iii)
par(mfrow=c(2,2))
plot(fit1)
library(lmtest)
bptest(fit1)

shapiro.test(fit1$residuals)

#iv)
std_data=data.frame(scale(df))
std_data
model1=lm(Volume~.,data=std_data)
model1
summary(model1)
