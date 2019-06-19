k1<-2
k2<-3
k3<-5
theta1<-2
theta2<-4

ma<-20


plot(seq(0,7,0.1),dgamma(seq(0,7,0.1),shape=1,scale=1),type="l",xla="x", ylab="y") 
lines(seq(0,7,0.01),dgamma(seq(0,7,0.01),shape=3,scale=0.5),lty=2,lwd=2)
lines(seq(0,7,0.1),dgamma(seq(0,7,0.1),shape=2,scale=1),lty=3,lwd=2) 

legend(4,0.8,c("Gamma(1,1)","Gamma(3,0.5)","Gamma(2,1)"),lty=c(1,2,3),bty="n",lwd=c(1,2,2))

hist(rgamma(10000,shape=3,scale=200),xlab="Ingreso",ylab="Individuos",main="")

