

x1<-seq(-3,3,0.01)
x2<-seq(-6,6,0.01)

plot(x2,dnorm(x2,0,2),type="l",ylim=c(0,0.4))
lines(x2,dnorm(x2,2,1),lty=2,lwd=2)
lines(x2,dnorm(x2,0,1),lty=3,lwd)
legend(-4,0.4,)

