set.seed(1)
pob<-rnorm(1000,5,1)
n<-10:300
est<-rep(NA,length(n))
for(i in 1:length(n)){
sam<-sample(pob,n[i])
est[i]<-mean(sam)
}

pob1<-rexp(1000,1/5)
n1<-10:300
est1<-rep(NA,length(n1))
for(i in 1:length(n1)){
sam1<-sample(pob1,n1[i])
est1[i]<-mean(sam1)
}

par(mfrow=c(2,1))
plot(n,est,type="l", xlab="n", ylab="Media muestral", main = "Población normal")
abline(5,0)

plot(n1,est1,type="l", xlab="n", ylab="Media muestral", main = "Población exponencial")
abline(5,0)
