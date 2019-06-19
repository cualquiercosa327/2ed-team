set.seed(1)

NG<-1000
n<-seq(2,100,1)
cont_N<-rep(0,length(n))
cont_T<-rep(0,length(n))
mu<-0
sigma<-1
alpha<-0.05
for(k in 2:100){
for(i in 1:NG){
dato<-rnorm(k,mu,sigma)
inf_N<-mean(dato)-qnorm(1-alpha/2)*sigma/sqrt(k)
sup_N<-mean(dato)+qnorm(1-alpha/2)*sigma/sqrt(k)
inf_T<-mean(dato)-qt(1-alpha/2,k-1)*sqrt(var(dato))/sqrt(k)
sup_T<-mean(dato)+qt(1-alpha/2,k-1)*sqrt(var(dato))/sqrt(k)
if(inf_N<mu&&mu<sup_N){cont_N[k-1]<-cont_N[k-1]+1}
if(inf_T<mu&&mu<sup_T){cont_T[k-1]<-cont_T[k-1]+1}
}
}
cont_N
cont_T     

plot(cont_N/NG,type="l",col="blue",ylab="Cobertura real",xlab="n")
lines(cont_T/NG,col="red")
abline(0.95,0)
legend(40,0.93,c("Intervalo t", "Intervalo normal"),lty=c(1,1),col=c("red","blue"))
