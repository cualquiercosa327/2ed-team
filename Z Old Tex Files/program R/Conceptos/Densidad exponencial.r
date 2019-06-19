plot(seq(0,10,0.1),dexp(seq(0,10,0.1),rate=1/2),type="l",xla="x", ylab="y")
lines(seq(0,10,0.01),dexp(seq(0,10,0.01),rate=1/3),lty=2,lwd=2)
lines(seq(0,10,0.1),dexp(seq(0,10,0.1),rate=1/5),lty=3,lwd=2)
legend(6,0.4,c("Exp(2)","Exp(3)","Exp(5)"),lty=c(1,2,3),bty="n",lwd=c(1,2,2))
