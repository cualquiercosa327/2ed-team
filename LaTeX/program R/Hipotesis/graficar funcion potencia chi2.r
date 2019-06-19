po_sigma<-function(sigma,n,sigma0,alpha){
1-pchisq(sigma0/sigma*qchisq(1-alpha/2,n),n)+pchisq(sigma0/sigma*qchisq(alpha/2,n),n)
}

#----------------------------------------------------------------
## Función de potencia para sigma2=sigma20 vs, sigma diferente de sigma0
#----------------------------------------------------------------

alpha<-0.05
sigma0<-10
n1<-10
n2<-30
n3<-50

sigma<-sigma0+seq(-10,30,0.1)

plot(function(x) po_sigma(x,n1,sigma0,alpha),0,40,type="l",xlab="sigma^2",ylab="función de potencia")
curve(po_sigma(x,n2,sigma0,alpha),0,40,lty=2,add=T)
curve(po_sigma(x,n3,sigma0,alpha),0,40,lty=3,add=T)
legend(30,0.4,c("n=10","n=30","n=50"),lty=c(1,2,3))

#----------------------------------------------------------------
## Función de potencia para sigma2=sigma20 vs, sigma > sigma0
#----------------------------------------------------------------

po_sigma_1<-function(sigma,n,sigma0,alpha){
1-pchisq(sigma0/sigma*qchisq(1-alpha,n),n)
}

alpha<-0.05
sigma0<-10
n1<-10
n2<-30
n3<-50

sigma<-sigma0+seq(-10,30,0.1)

beta1<-1-pchisq(sigma0/sigma*qchisq(1-alpha,n1),n1)
beta2<-1-pchisq(sigma0/sigma*qchisq(1-alpha,n2),n2)
beta3<-1-pchisq(sigma0/sigma*qchisq(1-alpha,n3),n3)

plot(function(x) po_sigma_1(x,n3,sigma0,alpha),0,40,type="l",xlab="sigma^2",ylab="función de potencia",lty=1)
curve(po_sigma_1(x,n2,sigma0,alpha),0,40,lty=2,add=T)
curve(po_sigma_1(x,n1,sigma0,alpha),0,40,lty=3,add=T)
legend(30,0.4,c("n=10","n=30","n=50"),lty=c(3,2,1))


