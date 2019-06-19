set.seed(1)

NG<-1000 ## iteraciones
n<-seq(5,200,1)

cont_N<-rep(0,length(n))
cont_G<-rep(0,length(n))

l_N<-rep(NA,length(n))
l_G<-rep(NA,length(n))

theta<-2
alpha<-0.05

for(k in 1:length(n)){
aux_N<-rep(NA,NG)
aux_G<-rep(NA,NG)
for(i in 1:NG){
dato<-rexp(k,1/theta)
inf_N<-mean(dato)*sqrt(n[k])/(qnorm(1-alpha/2)+sqrt(n[k]))
sup_N<-mean(dato)*sqrt(n[k])/(-qnorm(1-alpha/2)+sqrt(n[k]))
aux_N[i]<-sup_N-inf_N

inf_G<-sum(dato)/qgamma(1-alpha/2,shape=n[k],scale=1)
sup_G<-sum(dato)/qgamma(alpha/2,shape=n[k],scale=1)
aux_G[i]<-sup_G-inf_G

if(inf_N<theta&&theta<sup_N){cont_N[k]<-cont_N[k]+1}
if(inf_G<theta&&theta<sup_G){cont_G[k]<-cont_G[k]+1}
}
l_N[k]<-mean(aux_N)
l_G[k]<-mean(aux_G)
}
cont_N
cont_G

plot(cont_N/NG,type="l",col="blue",ylab="Cobertura real",xlab="n")
lines(cont_G/NG,col="red")
abline(0.95,0)
legend(100,0.85,c("Intervalo Gamma", "Intervalo normal"),lty=c(1,1),col=c("red","blue"))

plot(l_N,type="l",col="blue",xlab="n",ylab="longitud")
lines(l_G,col="red")
legend(100,10,c("Intervalo Gamma", "Intervalo normal"),lty=c(1,1),col=c("red","blue"))
