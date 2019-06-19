pote_2_norm<-function(mux,muy){
1-pnorm((mux-muy)/sqrt((sigmaX/nx)+(sigmaY/ny))+qnorm(1-alpha/2))+pnorm((mux-muy)/sqrt((sigmaX/nx)+(sigmaY/ny))-qnorm(1-alpha/2))
}

sigmaX<-1
sigmaY<-4
nx<-ny<-10
alpha<-0.05

muX<-muY<-seq(-2,2,0.1)
potencia<- outer(muX, muY, pote_2_norm)

op <- par(bg = "white")

persp(muX, muY, potencia, theta = 20, phi = 20, expand = 0.8)

#----------------------------------------------------------------
## Función de potencia para mu=mu0 vs, mu diferente de mu0
#----------------------------------------------------------------

pote_2_norm<-function(dif,nx,ny,alpha,sigmaX,sigmaY){
1-pnorm(dif/sqrt((sigmaX/nx)+(sigmaY/ny))+qnorm(1-alpha/2))+pnorm(dif/sqrt((sigmaX/nx)+(sigmaY/ny))-qnorm(1-alpha/2))
}


alpha<-0.05
sigmaX<-1
sigmaY<-4
n1<-10
n2<-30
n3<-50

plot(function(x) pote_2_norm(x,n1,n1,alpha,sigmaX,sigmaY),-4,4,type="l",xlab="mu_X-mu_Y",ylab="función de potencia")
curve(pote_2_norm(x,n2,n2,alpha,sigmaX,sigmaY),-4,4,lty=2,add=T)
curve(pote_2_norm(x,n3,n3,alpha,sigmaX,sigmaY),-4,4,lty=3,add=T)
legend(1.8,0.4,c("nx=ny=10","nx=ny=30","nx=ny=50"),lty=c(1,2,3))
