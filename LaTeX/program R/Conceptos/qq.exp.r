set.seed(01234567)
a<-rexp(20,1/50)

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

par(mfrow=c(1,2))

#a<-c(55,  23,  84,  35,  65,   6,   7,  32,  20,   4,   2,   4, 121,  59,  60,   0,  80,  36,  29,  63)

qq.exp(a)
qq.exp.line(a)

qqnorm(a,main="QQ plot normal")
qqline(a)



