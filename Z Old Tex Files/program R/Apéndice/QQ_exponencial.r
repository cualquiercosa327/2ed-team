qq.exp<-function(y){
y<-sort(y)
n<-length(y)
j<-c(1:n)
percen<--log(1-(j-0.5)/n)
plot(percen,y,xlab="",ylab="",main="QQ plot exponencial")
}

qq.exp.line<-function(y){
y<-sort(y)
n<-length(y)
j<-c(1:n)
percen<--log(1-(j-0.5)/n)
abline(0,1/lm(percen ~ y-1)$coef)
return(1/lm(percen ~ y-1)$coef)
}

set.seed(1234)
x1<-rexp(10,5)
x2<-rexp(50,5)
par(mfrow=c(1,2))
qq.exp(x1)
qq.exp.line(x1)
qq.exp(x2)
qq.exp.line(x2)

n<-10
y1<-runif(n,3,5)
y2<-rgamma(n,3,5)
y3<-rnorm(n,10,2)

par(mfrow=c(1,3))
qq.exp(y1)
qq.exp.line(y1)
qq.exp(y2)
qq.exp.line(y2)
qq.exp(y3)
qq.exp.line(y3)




a<-c(75, 87, 83, 73, 74, 88, 88,74, 64, 92, 73, 87, 91, 83 , 84)
 qq.exp(a)
qq.exp.line(a)

