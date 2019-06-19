#----------------------------------------------------------------
## Función de potencia para mu=mu0 vs, mu diferente de mu0
#----------------------------------------------------------------

alpha<-0.05
sigma<-1
mu0<-2
n<-35

mu<-2+seq(0,1.2,0.05)

beta<-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha))

plot(beta,type="l",xaxt="n",xlab="mu",ylab="Probabilidad de cometer error tipo II",ylim=c(0,1))
axis(1, 1:length(mu), mu)                  



