pote_sigX_sigY<-function(cociente,nx,ny,alpha){
1-pf(cociente*qf(1-alpha/2,nx-1,ny-1),nx-1,ny-1)+pf(cociente*qf(alpha/2,nx-1,ny-1),nx-1,ny-1)
}
alpha<-0.05
n1<-10
n2<-30
n3<-50

plot(function(x) pote_sigX_sigY(x,n3,n3,alpha),0,10,type="l",xlab="sig_X/sig_Y",ylab="función de potencia")
curve(pote_sigX_sigY(x,n2,n2,alpha),0,10,lty=2,add=T)
curve(pote_sigX_sigY(x,n1,n1,alpha),0,10,lty=3,add=T)
legend(6,0.4,c("nx=ny=10","nx=ny=30","nx=ny=50"),lty=c(3,2,1))

