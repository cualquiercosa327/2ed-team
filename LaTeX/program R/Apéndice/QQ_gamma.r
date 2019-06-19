
n<-50
y<-rweibull(n,10,20)


the=var(y)/mean(y)
k=(mean(y))^2/var(y)
plot(qgamma((c(1:n)-0.5)/n,shape=k,scale=the),sort(y),xlab="Percentiles muestrales", ylab="percentiles teóricos")

abline(0,1)



set.seed(12345678)
x<-rnorm(30,0.5,0.3)
n<-length(x)
va<-var(x)*(n-1)/n
 bar<-mean(x)
 a<-bar^2*(1-bar)/va-bar
 b<-(1-bar)*(bar*(1-bar)/va-1)
 par(mfrow=c(1,2))
 plot(qbeta((c(1:n)-0.5)/n,a,b),sort(x),xlab="Percentiles
 muestrales", ylab="percentiles teóricos",main="QQ plot Beta")
 abline(0,1)
 qqnorm(x)
 qqline(x)
 
