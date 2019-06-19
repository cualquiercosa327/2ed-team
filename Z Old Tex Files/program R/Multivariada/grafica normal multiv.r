rm(list=ls())


mu1<-1
mu2<-2
## las varianzas
sig1<-1
sig2<-4
sig12<--1
rho<-sig12/(sqrt(sig1*sig2))


x<-seq(-2.5,5.5,0.1)
y<-seq(-2,4,0.1)


f<-function(x,y){
fun<-exp(-(((x-mu1)^2)/sig1+((y-mu2)^2)/sig2-2*rho*(x-mu1)*(y-mu2)/sqrt(sig1*sig2))/(2*(1-rho^2)))
fun/(2*pi*sqrt(sig1*sig2)*sqrt(1-rho^2))
}

z<- outer(x, y, f)

op <- par(bg = "white")

persp(x, y, z, theta = 20, phi = 20, expand = 0.8)

