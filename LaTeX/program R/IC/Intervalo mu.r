### longitud del intervalo t para mu
set.seed(123)

alpha<-0.05
mu<-0
sigma<-1
m<-1000
n<-seq(3,100,1)
l_t<-rep(NA,length(n))
for(i in 1:length(n)){
aux<-rep(NA,m)
for(k in 1:m){
dato<-rnorm(n[i],mean=0,sd=sigma)
aux[k]<-2*qt(1-alpha/2,n[i]-1)*sqrt(var(dato))/sqrt(n[i])
}
l_t[i]<-mean(aux)
}


l_n<-2*qnorm(1-alpha/2)*sigma/sqrt(n)

plot(l_t,type="l",xlab="n",ylab="longitud")
lines(l_n,lty=2)
legend(60,4,c("Intervalo t", "Intervalo normal"),lty=c(1,2))

