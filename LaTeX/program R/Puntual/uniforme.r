set.seed(1)
n<-5:300
theta<-3
est.MV<-rep(NA,length(n))
est.mom<-rep(NA,length(n))
for(i in 1:length(n)){
x<-runif(n[i],-theta,theta)
est.mom[i]<-sqrt(3*var(x)*(n[i]-1)/n[i])
est.MV[i]<-max(-min(x),max(x))
}


plot(n,est.mom,xlab="n",ylab="Estimación",main="Uniforme(-3,3)",type="l",col="blue")
abline(3,0)
lines(n,est.MV,col="red")
legend(200,2.6,c("Momentos","MV"),lty=c(1,1),col=c("blue","red"),box.col=0)


