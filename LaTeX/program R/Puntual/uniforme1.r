set.seed(1)
n<-5:300
theta1<-3
theta2<-5


est.MV<-rep(NA,length(n))
est.mom<-rep(NA,length(n))
for(i in 1:length(n)){
x<-runif(n[i],0,theta1)
est.mom[i]<-mean(x)*2
est.MV[i]<-max(x)
}

est1.MV<-rep(NA,length(n))
est1.mom<-rep(NA,length(n))
for(i in 1:length(n)){
x<-runif(n[i],0,theta2)
est1.mom[i]<-mean(x)*2
est1.MV[i]<-max(x)
}



par(mfrow=c(2,1))

plot(n,est.mom,xlab="n",ylab="Estimación",main="Uniforme(0,3)",type="l",col="blue")
abline(3,0)
lines(n,est.MV,col="red")
legend(200,2.6,c("Momentos","MV"),lty=c(1,1),col=c("blue","red"),box.col=0)

plot(n,est1.mom,xlab="n",ylab="Estimación",main="Uniforme(0,5)",type="l",col="blue")
abline(5,0)
lines(n,est1.MV,col="red")
legend(200,4.6,c("Momentos","MV"),lty=c(1,1),col=c("blue","red"),box.col=0)


