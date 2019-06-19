set.seed(1234)

n1<-50
n2<-100
n3<-200
n4<-500

x1<-rnorm(n1,3,1)
x2<-rnorm(n2,3,1)
x3<-rnorm(n3,3,1)
x4<-rnorm(n4,3,1)

par(mfrow=c(2,2))

hist(x1,freq=F)
lines(dnorm(seq(0,6,0.01),3,1))
hist(x2,freq=F)
lines(dnorm(seq(0,6,0.01),3,1))
hist(x3,freq=F)
lines(dnorm(seq(0,6,0.01),3,1))
hist(x4,freq=F)
lines(dnorm(seq(0,6,0.01),3,1))

