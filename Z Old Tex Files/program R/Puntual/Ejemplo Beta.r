x<-c(0.7, 1.4, 19.7, 0.1, 12.4, 1.1, 0.5, 18.9, 5.0, 0.3, 0.6, 5.4, 6.7, 0.9)/100
n<-length(x)
va<-var(x)*(n-1)/n
bar<-mean(x)
a<-bar^2*(1-bar)/va-bar
b<-(1-bar)*(bar*(1-bar)/va-1)

hist(x,breaks=20,freq=F)
curve(dbeta(x,a,b),add=T)


hist(x,breaks=20,freq=F)
curve(dexp(x,rate=1/mean(x)),add=T)



### QQ plot exponencial

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
return(lm(percen ~ y-1)$coef)
}

qq.exp(x)
qq.exp.line(x)



