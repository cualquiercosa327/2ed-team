#   t central

x<-seq(-4,4,0.01)
n1<-2
n2<-10
n3<-20

plot(x,dnorm(x,0,1),type="l",xlab="x",ylab="y",lwd=2)
lines(x,dt(x,n1),lty=2,lwd=2)
lines(x,dt(x,n2),lty=3,lwd=2)
lines(x,dt(x,n3),lty=4,lwd=2)

legend(1.1,0.35,c("normal estándar","n=2","n=10","n=20"),lty=c(1,2,3,4),lwd=c(2,2,2,2),bty="n")




#   t no central

x<-seq(-3,8,0.005)
n1<-5
n2<-30
mu1<-1
mu2<-3

plot(x,dt(x,df=n2,ncp=mu1),type="l",xlab="x",ylab="y",lwd=2)
lines(x,dt(x,df=n1,ncp=mu1),lty=2,lwd=2)
lines(x,dt(x,df=n1,ncp=mu2),lty=4,lwd=2)
lines(x,dt(x,df=n2,ncp=mu2),lty=3,lwd=2)

legend(5,0.35,c("t(30,1)","t(10,1)","t(30,3)","t(10,3)"),lty=c(1,2,3,4),lwd=c(2,2,2,2),bty="n")
