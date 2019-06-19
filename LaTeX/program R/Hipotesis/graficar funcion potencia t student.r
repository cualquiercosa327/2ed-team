#----------------------------------------------------------------
## Función de potencia para mu=mu0 vs, mu diferente de mu0 con sigma desconocida
#----------------------------------------------------------------
po_t_nocen<-function(mu,n,mu0,alpha,sigma){
delta<-(mu-mu0)*sqrt(n)/sigma  
1-pt(qt(1-alpha/2,n-1),df=n-1,ncp=delta)+pt(qt(alpha/2,n-1),df=n-1,ncp=delta)
}

alpha<-0.05
sigma<-1
mu0<-2
n1<-10
n2<-30
n3<-50


mu<-mu0+seq(-2,2,0.05)

plot(po_t_nocen(mu,n1,mu0,alpha,sigma),type="l",xaxt="n",xlab="mu",ylab="función de potencia")
axis(1, 1:length(mu), mu)
lines(po_t_nocen(mu,n2,mu0,alpha,sigma),lty=2)
lines(po_t_nocen(mu,n3,mu0,alpha,sigma),lty=3)
legend(60,0.4,c("n=10","n=30","n=50"),lty=c(1,2,3))


#----------------------------------------------------------------
## Comparación de función de potencia para mu=mu0 vs, mu diferente de mu0 para diferentes alpha
#----------------------------------------------------------------

alpha1<-0.03
alpha2<-0.05
alpha3<-0.1
sigma<-1
mu0<-2
n<-20

mu<-mu0+seq(-2,2,0.05)


plot(po_t_nocen(mu,n,mu0,alpha1,sigma),type="l",xaxt="n",xlab="mu",ylab="función de potencia")
axis(1, 1:length(mu), mu)
lines(po_t_nocen(mu,n,mu0,alpha2,sigma),lty=2)
lines(po_t_nocen(mu,n,mu0,alpha3,sigma),lty=3)
legend(55,0.3,c("alpha=0.03","alpha=0.05","alpha=0.1"),lty=c(1,2,3))
