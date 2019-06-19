x<-seq(0,5,0.01)

plot(x,df(x,2,2),type="l",xlab="x",ylab="y",lwd=2)
lines(x,df(x,2,5),lty=4,lwd=2)
lines(x,df(x,10,5),lty=3,lwd=2)
lines(x,df(x,10,10),lty=2,lwd=2)
legend(3,0.8,c("F(2,2)","F(2,5)","F(10,5)","F(10,10)"),lty=c(1,4,3,2),lwd=c(2,2,2,2),bty="n")