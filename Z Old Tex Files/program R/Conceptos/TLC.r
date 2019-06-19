set.seed(1234)

#######################################
#######################################

n<-c(5,10,30,50,100,500)
m<-500
lambda<-3
par(mfrow=c(3,2))
for(i in 1:length(n)){
y<-rep(NA,m)
for(j in 1:m){
y[j]<-sqrt(n[i])*(mean(rpois(n[i],lambda))-lambda)/(sqrt(lambda))}
a<-n[i]
hist(y,freq=F,main=a,ylab="Frecuencia",xlab="")
curve(dnorm(x),lty=2,lwd=2,add=T)
}


#######################################
#######################################

n<-c(5,10,30,50,100,500)
m<-500
alpha<-3
gamm<-2
par(mfrow=c(3,2))
for(i in 1:length(n)){
y<-rep(NA,m)
for(j in 1:m){
y[j]<-sqrt(n[i])*(mean(rgamma(n[i],alpha,1/gamm))-(alpha*gamm))/(sqrt(alpha*(gamm^2)))
}
a<-n[i]
hist(y,freq=F,main=a,ylab="Frecuencia",xlab="")
curve(dnorm(x),lty=2,lwd=2,add=T)
}


