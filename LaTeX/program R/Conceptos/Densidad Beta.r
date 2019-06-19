par(mfrow=c(2,1))
f11<-dbeta(seq(0.01,0.99,0.01),shape1=0.5,shape2=1)
f21<-dbeta(seq(0.01,0.99,0.01),shape1=0.5,shape2=0.3)
plot(f11,type="l",ylim=c(0,max(c(f11,f21))),xaxt="n",xlab="x",ylab="f(x)")
lines(f21,lty=2)
axis(1,1:length(seq(0.01,0.99,0.01)),seq(0.01,0.99,0.01))
legend(35,5,c("Beta(0.5,1)","Beta(0.5,0.3)"),lty=c(1,2),bty="n")


f12<-dbeta(seq(0.01,0.99,0.01),shape1=2,shape2=1)
f22<-dbeta(seq(0.01,0.99,0.01),shape1=2,shape2=3)
plot(f12,type="l",ylim=c(0,max(c(f12,f22))),xaxt="n",xlab="x",ylab="f(x)")
lines(f22,lty=2)
axis(1,1:length(seq(0.01,0.99,0.01)),seq(0.01,0.99,0.01))
legend(70,1.6,c("Beta(2,1)","Beta(2,3)"),lty=c(1,2),bty="n")






hist(rbeta(10000,shape1=3,shape2=200),xlab="Ingreso",ylab="Individuos",main="")

