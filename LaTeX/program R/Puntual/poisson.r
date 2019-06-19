set.seed(123)
n<-5:300
est.mean<-rep(NA,length(n))
est.var<-rep(NA,length(n))
for(i in 1:length(n)){
x<-rpois(n[i],5)
est.mean[i]<-mean(x)
est.var[i]<-var(x)*(n[i]-1)/n[i]
}

est1.mean<-rep(NA,length(n))
est1.var<-rep(NA,length(n))
for(i in 1:length(n)){
y<-rpois(n[i],15)
est1.mean[i]<-mean(y)
est1.var[i]<-var(y)*(n[i]-1)/n[i]
}


par(mfrow=c(2,1))
plot(n,est.var,xlab="n",ylab="Estimación",main="Población P(5)",type="l",col="blue",ylim=c(2,9))
abline(5,0)
lines(n,est.mean,col="red")
legend(200,9.2,c("Media","Varianza"),lty=c(1,1),col=c("red","blue"),box.col=0)

plot(n,est1.var,xlab="n",ylab="Estimación",main="Población P(15)",type="l",col="blue")
abline(15,0)
lines(n,est1.mean,col="red")
legend(200,45,c("Media","Varianza"),lty=c(1,1),col=c("red","blue"),box.col=0)

