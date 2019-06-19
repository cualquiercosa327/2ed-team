set.seed(1)
n<-5:300
theta1<-3
theta2<-5


est.MV1<-est.MV2<-rep(NA,length(n))
est.mom1<-est.mom2<-rep(NA,length(n))
for(i in 1:length(n)){
x<-runif(n[i],theta1,theta2)
est.mom1[i]<-mean(x)-sqrt(3)*sqrt(var(x)*(n[i]-1)/n[i])
est.mom2[i]<-mean(x)+sqrt(3)*sqrt(var(x)*(n[i]-1)/n[i])
est.MV1[i]<-min(x)
est.MV2[i]<-max(x)
}



par(mfrow=c(2,1))

plot(n,est.mom1,xlab="n",ylab="Estimación",main="Estimaciones de theta_1",type="l",col="blue")
abline(theta1,0)
lines(n,est.MV1,col="red")
legend(200,3.4,c("Momentos","MV"),lty=c(1,1),col=c("blue","red"),box.col=0)


plot(n,est.mom2,xlab="n",ylab="Estimación",main="Estimaciones de theta_2",type="l",col="blue")
abline(theta2,0)
lines(n,est.MV2,col="red")
legend(200,4.8,c("Momentos","MV"),lty=c(1,1),col=c("blue","red"),box.col=0)


