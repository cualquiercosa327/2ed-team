a<-c(0.7,-1.6,-0.2,-1.2,-1,3.4,3.7,0.8,0,2)
b<-c(1.9,0.8,1.1,0.1,-0.1,4.4,5.5,1.6,4.6,3.4)
length(a)
length(b)


par(mfrow=c(1,2))
qqnorm(a,main="Sedante A")
qqline(a)
qqnorm(b,main="Sedante B")
qqline(b)

x<-matrix(c(a,b),10,2)
var(x)
cor(x)

## Independencia entre las dos variables

alpha<-0.5
p<-2
n<-10
esta<--n*log(det(cor(x)))
perce<-qchisq(1-alpha,p*(p+1)/2-p)
p.val<-pchisq(esta,p*(p+1)/2-p,lower.tail=F)
esta
perce
p.val



