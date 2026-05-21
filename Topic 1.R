library(MASS)
x=matrix(c(1,1,0,0,1,0,
           1,1,0,0,0,1,
           1,0,1,0,1,0,
           1,0,1,0,0,1,
           1,0,0,1,1,0,
           1,0,0,1,0,1),nrow = 6,byrow=T)
x
n=6
p=6
r=qr(x)$rank
ifelse(r<p,"Non full rank model","Full rank model")
l1=matrix(c(0,1,1,0,0,0),nrow=6)
s=t(x)%*%x
G=ginv(s)
H=s%*%G
t(l1)
round(t(l1)%*%H,10)
all(t(l1)==round(t(l1)%*%H,10))

#i)
l2=matrix(c(0,0,0,0,1,-1),nrow=6)
t(l2)
round(t(l2)%*%H,10)
all(t(l2)==round(t(l2)%*%H,10))

#ii)
l3=matrix(c(1,1,0,0,0,0),nrow=6)
t(l3)
round(t(l3)%*%H,10)
all(t(l3)==round(t(l3)%*%H,10))

#iii)
l4=matrix(c(6,2,2,2,3,3),nrow=6)
t(l4)
round(t(l4)%*%H,10)
all(t(l4)==round(t(l4)%*%H,10))

#iv)
l5=matrix(c(0,1,-2,1,0,0),nrow=6)
t(l5)
round(t(l5)%*%H,10)
all(t(l5)==round(t(l5)%*%H,10))

l6=matrix(c(0,1,-1,0,0,0),nrow=6)
t(l6)
round(t(l6)%*%H,10)
all(t(l6)==round(t(l6)%*%H,10))

y=c(1.6,2.8,1.3,4,4.2,1.8)
beta_hat=G%*%t(x)%*%y
e=y-x%*%beta_hat
SSE=t(e)%*%e
sigma2_hat=SSE/(n-r)
sigma2_hat
cov_mat=t(l2)%*%G%*%l6%*%sigma2_hat
cov_mat

x=matrix(c(1,0,0,0,1,0,0,0,
           1,0,0,0,0,1,0,0,
           1,0,0,0,0,0,0,1,
           0,1,0,0,0,1,0,0,
           0,1,0,0,0,0,1,0,
           0,1,0,0,0,0,0,1,
           0,0,1,0,0,1,0,0,
           0,0,1,0,0,1,0,0,
           0,0,1,0,0,0,1,0,
           0,0,0,1,1,0,0,0,
           0,0,0,1,0,0,1,0,
           0,0,0,1,0,0,0,1),nrow = 12,ncol = 8,byrow = T)
n=12
p=8
r=qr(x)$rank
r
ifelse(r<p,"Non full rank model","Full rank model")
s=t(x)%*%x
G=ginv(s)
H=s%*%G

#i)
l1=matrix(c(1,0,0,0,0,0,1,0),nrow=8)
t(l1)
round(t(l1)%*%H,10)
all(t(l1)==round(t(l1)%*%H,10))

#ii)
l2=matrix(c(1,1,1,-3,0,0,0,0),nrow=8)
t(l2)
round(t(l2)%*%H,10) 
all(t(l2)==round(t(l2)%*%H,10))

#iii)
l3=matrix(c(0,3,0,0,0,1,1,1),nrow=8)
t(l3)
round(t(l3)%*%H,10)
all(t(l3)==round(t(l3)%*%H,10))

#iv)
l4=matrix(c(1,0,0,-2,0,0,1,0),nrow=8)
t(l4)
round(t(l4)%*%H,10)
all(t(l4)==round(t(l4)%*%H,10))

y=c(73,74,71,75,67,72,73,75,68,75,72,75)
beta_hat=G%*%t(x)%*%y
e=y-x%*%beta_hat
SSE=t(e)%*%e
sigma2_hat=SSE/(n-r)
sigma2_hat
var_=t(l1)%*%G%*%l1%*%sigma2_hat
var_

#Q3)
y=rnorm(100,mean=50,sd=4)
p1=2 # No. of unknwon parameter (mu,sigma)
n=length(p) # No. of observed responses
p=c(0.2,0.4,0.6,0.8)
z_p=quantile(y,probs = p)
z_p=as.matrix(z_p)
phi_inv=qnorm(p)
phi_inv
X <- matrix(c(rep(1,n),phi_inv),ncol=2,byrow=F)
X
r=qr(X)$rank
r
s=t(X)%*%X
beta_hat=solve(s)%*%t(X)%*%z_p
beta_hat
e=z_p-X%*%beta_hat
SSE=t(e)%*%e
sigma2_hat=SSE/(n-r)
sigma2_hat
var_=solve(s)*as.numeric(sigma2_hat)
var_
