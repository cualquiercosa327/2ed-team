qq.wei<-function(y){
y<-sort(y)
n<-length(y)
j<-c(1:n)
percen<-log(-log(1-(j-0.5)/n))

inte<-lm(log(y)~percen)$coef[1]
pend<-lm(log(y)~percen)$coef[2]
plot(percen,log(y),xlab="",ylab="",main="QQ plot Weibull")
abline(inte,pend)
thet<-exp(inte)

k<-1/pend
list("theta"=thet, "k"=k)
}

set.seed(1234)
x1<-rweibull(15,shape=2,scale=10)
x2<-rweibull(50,shape=2,scale=10)
par(mfrow=c(1,2))
qq.wei(x1)
qq.wei(x2)


set.seed(1234)
n<-50
y1<-runif(n,3,5)
y2<-rgamma(n,3,5)
y3<-rnorm(n,10,2)

par(mfrow=c(1,3))
qq.wei(y1)
qq.wei(y2)
qq.wei(y3)


