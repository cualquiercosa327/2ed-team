###############################
#### Para p pequeño ###########
###############################

n1<-10
n2<-50
n3<-100
p1<-0.2
p2<-0.5
p3<-0.8


a11<-dbinom(c(0:n1),n1,p1)
a12<-dbinom(c(0:n1),n1,p2)
a13<-dbinom(c(0:n1),n1,p3)

a21<-dbinom(c(0:n2),n2,p1)
a22<-dbinom(c(0:n2),n2,p2)
a23<-dbinom(c(0:n2),n2,p3)

a31<-dbinom(c(0:n3),n3,p1)
a32<-dbinom(c(0:n3),n3,p2)
a33<-dbinom(c(0:n3),n3,p3)

par(mfrow=c(3,3))

plot(a11,type="h",ylab="n=10",xlab="",main="p=0.2")
lines(dpois(c(0:n1),n1*p1),col=2)

plot(a12,type="h",ylab="n=10",xlab="",main="p=0.5")
lines(dpois(c(0:n1),n1*p2),col=2)

plot(a13,type="h",ylab="n=10",xlab="",main="p=0.8")
lines(dpois(c(0:n1),n1*p3),col=2)



plot(a21,type="h",ylab="n=50",xlab="")
lines(dpois(c(0:n2),n2*p1),col=2)

plot(a22,type="h",ylab="n=50",xlab="")
lines(dpois(c(0:n2),n2*p2),col=2)

plot(a23,type="h",ylab="n=50",xlab="")
lines(dpois(c(0:n2),n2*p3),col=2)



plot(a31,type="h",ylab="n=100",xlab="")
lines(dpois(c(0:n3),n3*p1),col=2)

plot(a32,type="h",ylab="n=100",xlab="")
lines(dpois(c(0:n3),n3*p2),col=2)

plot(a33,type="h",ylab="n=100",xlab="")
lines(dpois(c(0:n3),n3*p3),col=2)


##############################
#### Para p grande ###########
##############################

n1<-10
n2<-50
n3<-100

p1<-0.5
p2<-0.7
p3<-0.9

lam11<-n1*p1
lam12<-n1*p2
lam13<-n1*p3

lam21<-n2*p1
lam22<-n2*p2
lam23<-n2*p3

lam31<-n3*p1
lam32<-n3*p2
lam33<-n3*p3


b11<-dbinom(c(0:n1),n1,p1)
b12<-dbinom(c(0:n1),n1,p2)
b13<-dbinom(c(0:n1),n1,p3)

b21<-dbinom(c(0:n2),n2,p1)
b22<-dbinom(c(0:n2),n2,p2)
b23<-dbinom(c(0:n2),n2,p3)

b31<-dbinom(c(0:n3),n3,p1)
b32<-dbinom(c(0:n3),n3,p2)
b33<-dbinom(c(0:n3),n3,p3)


pois<-function(x,lambda,n){
dpois(x,lambda)*(factorial(x))*(lambda^(n-2*x))/(factorial(n-x))
}


par(mfrow=c(3,3))

plot(b11,type="h",ylab="n=10",xlab="",main="p=0.5")
lines(pois(c(0:n1),n1*(1-p1),n1),col=2)

plot(b12,type="h",ylab="n=10",xlab="",main="p=0.7")
lines(pois(c(0:n1),n1*(1-p2),n1),col=2)

plot(b13,type="h",ylab="n=10",xlab="",main="p=0.9")
lines(pois(c(0:n1),n1*(1-p3),n1),col=2)



plot(b21,type="h",ylab="n=50",xlab="")
lines(pois(c(0:n2),n2*(1-p1),n2),col=2)

plot(b22,type="h",ylab="n=50",xlab="")
lines(pois(c(0:n2),n2*(1-p2),n2),col=2)

plot(b23,type="h",ylab="n=50",xlab="")
lines(pois(c(0:n2),n2*(1-p3),n2),col=2)



plot(b31,type="h",ylab="n=100",xlab="")
lines(pois(c(0:n3),n3*(1-p1),n3),col=2)

plot(b32,type="h",ylab="n=100",xlab="")
lines(pois(c(0:n3),n3*(1-p2),n3),col=2)

plot(b33,type="h",ylab="n=100",xlab="")
lines(pois(c(0:n3),n3*(1-p3),n3),col=2)