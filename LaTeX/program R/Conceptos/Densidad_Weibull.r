
k1<-2
k2<-3
k3<-5
theta1<-2
theta2<-4



plot(seq(0,4,0.01),dweibull(seq(0,4,0.01),shape=3,scale=0.5),type="l",xla="x", ylab="y")
lines(seq(0,4,0.1),dweibull(seq(0,4,0.1),shape=1,scale=1),lty=2,lwd=2) 
lines(seq(0,4,0.1),dweibull(seq(0,4,0.1),shape=2,scale=1),lty=3,lwd=2) 

legend(2.4,2,c("Weibull(3,0.5)","Weibull(1,1)","Weibull(2,1)"),lty=c(1,2,3),bty="n",lwd=c(1,2,2))

hist(rgamma(10000,shape=3,scale=200),xlab="Ingreso",ylab="Individuos",main="")

