po_norm<-function(mu,n,mu0,alpha,sigma){
1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha/2))+pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha/2))
}

#----------------------------------------------------------------
## Función de potencia para mu=mu0 vs, mu diferente de mu0
#----------------------------------------------------------------

alpha<-0.05
sigma<-1
mu0<-2
n1<-10
n2<-30
n3<-50

plot(function(x) po_norm(x,n1,mu0,alpha,sigma),0,4,type="l",xlab="mu",ylab="función de potencia")
curve(po_norm(x,n2,mu0,alpha,sigma),0,4,lty=2,add=T)
curve(po_norm(x,n3,mu0,alpha,sigma),0,4,lty=3,add=T)
legend(3,0.4,c("n=10","n=30","n=50"),lty=c(1,2,3))

#----------------------------------------------------------------
## Comparación de función de potencia para mu=mu0 vs, mu diferente de mu0 para diferentes alpha
#----------------------------------------------------------------
windows()

alpha1<-0.03
alpha2<-0.05
alpha3<-0.1
sigma<-1
mu0<-2
n<-20

plot(function(x) po_norm(x,n,mu0,alpha1,sigma),0,4,type="l",xlab="mu",ylab="función de potencia")
curve(po_norm(x,n,mu0,alpha2,sigma),0,4,lty=2,add=T)
curve(po_norm(x,n,mu0,alpha3,sigma),0,4,lty=3,add=T)
legend(2.6,0.3,c("alpha=0.03","alpha=0.05","alpha=0.1"),lty=c(1,2,3))


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
## Función de potencia para mu=mu0 vs, mu > mu0
#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
po_norm_1<-function(mu,n,mu0,alpha,sigma){
1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha))
}

alpha<-0.05
sigma<-1
mu0<-2
n1<-10
n2<-30
n3<-50

plot(function(x) po_norm_1(x,n1,mu0,alpha,sigma),1,4,type="l",xlab="mu",ylab="función de potencia")
curve(po_norm_1(x,n2,mu0,alpha,sigma),1,4,lty=2,add=T)
curve(po_norm_1(x,n3,mu0,alpha,sigma),1,4,lty=3,add=T)
legend(2.6,0.3,c("n=10","n=30","n=50"),lty=c(1,2,3))

#----------------------------------------------------------------
## Comparación de función de potencia para mu=mu0 vs, mu diferente de mu0 para diferentes alpha
#----------------------------------------------------------------
windows()

alpha1<-0.03
alpha2<-0.05
alpha3<-0.1
sigma<-1
mu0<-2
n<-20

plot(function(x) po_norm_1(x,n,mu0,alpha1,sigma),1,4,type="l",xlab="mu",ylab="función de potencia")
curve(po_norm_1(x,n,mu0,alpha2,sigma),1,4,,lty=2,add=T)
curve(po_norm_1(x,n,mu0,alpha3,sigma),1,4,,lty=3,add=T)
legend(2.6,0.3,c("alpha=0.03","alpha=0.05","alpha=0.1"),lty=c(1,2,3))


## Error tipo II 
plot(function(x) 1-po_norm_1(x,n1,mu0,alpha,sigma),1,4,type="l",xlab="mu",ylab="Probabilidad de error tipo II")
curve(1-po_norm_1(x,n2,mu0,alpha,sigma),1,4,lty=2,add=T)
curve(1-po_norm_1(x,n3,mu0,alpha,sigma),1,4,lty=3,add=T)
legend(3,0.7,c("n=10","n=30","n=50"),lty=c(1,2,3))



z<-c(355, 350, 340, 345, 354,358, 350, 343, 349, 346, 351, 358, 342, 350, 356, 345, 349, 356, 354, 346.)
mean(z)