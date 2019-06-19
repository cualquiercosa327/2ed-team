set.seed(1234)
n<-100
u<-runif(100)
e<--log(1-u)
e1<-rexp(100,1)
par(mfrow=c(1,2))
hist(e,ylab="Frecuencia",main="(a)")
hist(e1,ylab="Frecuencia",main="(b)")