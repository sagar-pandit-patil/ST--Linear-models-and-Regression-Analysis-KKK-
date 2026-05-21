#Q1
#a)
R1=c(3.1,3.5,4.8,3.1,5.5,5.0,4.8,4.3,4.7,4.3,5.1,3.0,4.3,5.5,4.2,3.5,5.7)
R2=c(4.3,3.4,5.0,4.1,4.7,5.0,5.0,2.9,5.0)
R3=c(3.9,4.5,7.0,6.7,5.8,5.6,4.8,5.5,6.6,5.3,5.7,6.0)
p=boxplot(R1,R2,R3)
p

#b)
n1=17
n2=9
n3=12
n=n1+n2+n3
k=3
p=k+1
x=matrix(c(rep(1,n1+n2+n3),rep(1,n1),rep(0,n2+n3),rep(0,n1),rep(1,n2),rep(0,n3),rep(0,n1+n2),rep(1,n3)),ncol=4,byrow=FALSE)
x
s=t(x)%*%x
library(MASS)
library(Matrix)
r=qr(x)$rank
r
ifelse(r<p,"Non full rank model","Full rank model")
G=ginv(s)
H=G%*%s
# To check the estimability condition of alpha1-alpha2
l1=matrix(c(0,1,-1,0),nrow=4)
t(l1)
zapsmall(t(l1)%*%H)
# To check the estimability condition of mu+alpha1
l2=matrix(c(1,1,0,0),nrow=4)
t(l2)
zapsmall(t(l2)%*%H)

#c)
#To construct ANOVA table
Y=matrix(c(R1,R2,R3),nrow=n)
beta_hat=G%*%t(x)%*%Y
beta_hat
SST=t(beta_hat)%*%t(x)%*%Y - n*mean(Y)^2
e=Y-x%*%beta_hat
SSE=t(e)%*%e
TSS=SST+SSE
MST=SST/(r-1)
MSE=SSE/(n-r)
F_cal=MST/MSE
F_cal
F_table=qf(0.05,r-1,n-r,lower.tail = F)
F_table
ifelse(F_cal>F_table,"Reject H0","Fails to reject H0")
p_val=pf(F_cal,r-1,n-r,lower.tail = F)
ANOVA_table=data.frame(Source=c("Treatment","Error","Total"),
                       df=c(r-1,n-r,n-1),
                       SS=c(SST,SSE,TSS),
                       MS=c(MST,MSE,"-"),
                       F_=c(F_cal,"-","-"),
                       P_value=c(p_val,"-","-"))
ANOVA_table
Region=c(rep("R1",n1),rep("R2",n2),rep("R3",n3))
Model.fit=aov(Y~Region)
S=summary(Model.fit)
S
library(car)
leveneTest(Model.fit)
shapiro.test(e)
par(mfrow=c(2,2))
plot(Model.fit)
TukeyHSD(Model.fit,conf.level=0.95)

#Q2)
A=c(1020,1010,1030,1000)
B=c(1030,1040,1050,1030,1060)
C=c(990,980,970,960,970,980)
D=c(1040,1050,1030,1070)
n1=4
n2=5
n3=6
n4=4
n=n1+n2+n3+n4
k=4
p=k+1
x=matrix(c(rep(1,n),rep(1,n1),rep(0,n2+n3+n4),rep(0,n1),rep(1,n2),rep(0,n3+n4),rep(0,n1+n2),rep(1,n3),rep(0,n4),rep(0,n1+n2+n3),rep(1,n4)),ncol=5,byrow = F)
x
r=qr(x)$rank
r
ifelse(r<p,"Non full rank model","Full rank model")
s=t(x)%*%x
G=ginv(s)
Y=matrix(c(A,B,C,D),nrow=n)
beta_hat=G%*%t(x)%*%Y
beta_hat
SST=t(beta_hat)%*%t(x)%*%Y - n*mean(Y)^2
e=Y-x%*%beta_hat
SSE=t(e)%*%e
TSS=SST+SSE
MST=SST/(r-1)
MSE=SSE/(n-r)
F_cal=MST/MSE
F_cal
F_table=qf(0.05,r-1,n-r,lower.tail = F)
F_table
ifelse(F_cal>F_table,"Reject H0","Fails to reject H0")
p_val=pf(F_cal,r-1,n-r,lower.tail = F)
ANOVA_tab=data.frame(Source=c("Treatment","Error","Total"),
                     df=c(r-1,n-r,n-1),
                     SS=c(SST,SSE,TSS),
                     MS=c(MST,MSE,"-"),
                     F_=c(F_cal,"-","-"),
                     P_value=c(p_val,"-","-"))
ANOVA_tab
Region=c(rep("A",n1),rep("B",n2),rep("C",n3),rep("D",n4))
Model.fit=aov(Y~Region)
S=summary(Model.fit)
S
library(car)
leveneTest(Model.fit)
shapiro.test(e)
par(mfrow=c(2,2))
plot(Model.fit)
TukeyHSD(Model.fit,conf.level=0.95)
