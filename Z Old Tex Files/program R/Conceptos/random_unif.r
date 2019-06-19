set.seed(123)
n<-c(500,1000)
theta<-3
par(mfrow=c(1,2))
for(i in 1:length(n)){
a<-n[i]
hist(sample(theta,n[i],replace=TRUE),main="",xlab=a,ylab="Frecuencia")
}