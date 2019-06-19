set.seed(123)
n1<-10
n2<-50
x1<-rnorm(n1,5,2)
z1<-(x1-mean(x1))/sd(x1)

j1<-c(1:n1)
percen1<-(j1-0.5)/n1

x2<-rnorm(n2,5,2)
z2<-(x2-mean(x2))/sd(x2)

j2<-c(1:n2)
percen2<-(j2-0.5)/n2


par(mfrow=c(1,2))
plot(qnorm(percen1),sort(z1))
abline(0,1)

plot(qnorm(percen2),sort(z2))
abline(0,1)


windows()
par(mfrow=c(1,2))
qqnorm(x1)
qqline(x1)

qqnorm(x2)
qqline(x2) 

## potencia


qq.norm<-function(y){
y<-sort(y)
z<-(y-mean(y))/sd(y)
n<-length(y)
j<-c(1:n)
percen<-(j-0.5)/n
plot(qnorm(percen),z,xlab="",ylab="",main="QQ plot normal")
}


set.seed(123)
n<-30
y1<-runif(n,3,5)
y2<-rgamma(n,3,5)
y3<-rexp(n,2)

par(mfrow=c(1,3))
qq.norm(y1)
abline(0,1)
qq.norm(y2)
abline(0,1)
qq.norm(y3)
abline(0,1)