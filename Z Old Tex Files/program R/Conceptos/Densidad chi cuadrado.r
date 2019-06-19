x<-seq(0,50,0.01)
n1<-2
n2<-5
n3<-10
n4<-20

plot(x,dchisq(x,n1),type="l",xlab="x",ylab="y")
lines(x,dchisq(x,n2),lty=2,lwd=2)
lines(x,dchisq(x,n3),lty=3,lwd=2)
lines(x,dchisq(x,n4),lty=4,lwd=2)

legend(35,0.4,c("n=2","n=5","n=10","n=20"),lty=c(1,2,3,4),lwd=c(1,2,2,2),bty="n")

