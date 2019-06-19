n<-c(5:200)
alpha=0.05

LG<-LN<-rep(NA,length(n))

for(i in 1:length(n)){
LG[i]<-n[i]*(1/qgamma(alpha/2,shape=n[i],scale=1)-1/qgamma(1-alpha/2,shape=n[i],scale=1))
LN[i]<-sqrt(n[i])*(1/(sqrt(n[i])-qnorm(1-alpha/2))-1/(sqrt(n[i])+qnorm(1-alpha/2)))
}

plot(LN,type="l",col="blue",xlab="n",ylab="Longitud esperada")
lines(LG,col="red")
legend(100,5,c("Intervalo Gamma", "Intervalo normal"),lty=c(1,1),col=c("red","blue"))