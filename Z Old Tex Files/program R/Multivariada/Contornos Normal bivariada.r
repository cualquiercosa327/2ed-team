densidad<-function(x1,x2,mu1,mu2,sigma){
f<-matrix(NA,length(x1),length(x2))
for(i in 1:length(x1)){
for(j in 1:length(x2)){
f[i,j]<-1/(2*pi)*det(sigma)^{-0.5}*exp(-t(c(x1[i],x2[j])-c(mu1,mu2))%*%solve(sigma)%*%(c(x1[i],x2[j])-c(mu1,mu2))/2)
}
}
f
}

##
mu1<-mu2<-0

## las varianzas
sig1<-matrix(c(4,-4,-4,25),2,2)
x1<-seq(mu1-2*sig1[1,1],mu1+2*sig1[1,1],by=0.5)
y1<-seq(mu2-2*sig1[2,2],mu2+2*sig1[2,2],by=0.5)
f1<-densidad(x1,y1,mu1,mu2,sig1)
rho1=sig1[1,2]/(sqrt(sig1[1,1])*sqrt(sig1[2,2]))

## las varianzas
sig2<-matrix(c(4,4,4,25),2,2)
x2<-seq(mu1-2*sig2[1,1],mu1+2*sig2[1,1],by=0.5)
y2<-seq(mu2-2*sig2[2,2],mu2+2*sig2[2,2],by=0.5)
f2<-densidad(x2,y2,mu1,mu2,sig2)
rho2=sig2[1,2]/(sqrt(sig2[1,1])*sqrt(sig2[2,2]))


## las varianzas
sig3<-matrix(c(4,-2,-2,25),2,2)
x3<-seq(mu1-2*sig3[1,1],mu1+2*sig3[1,1],by=0.5)
y3<-seq(mu2-2*sig3[2,2],mu2+2*sig3[2,2],by=0.5)
f3<-densidad(x3,y3,mu1,mu2,sig3)
rho3=sig3[1,2]/(sqrt(sig3[1,1])*sqrt(sig3[2,2]))


## las varianzas
sig4<-matrix(c(4,2,2,25),2,2)
x4<-seq(mu1-2*sig4[1,1],mu1+2*sig4[1,1],by=0.5)
y4<-seq(mu2-2*sig4[2,2],mu2+2*sig4[2,2],by=0.5)
f4<-densidad(x4,y4,mu1,mu2,sig4)
rho4=sig4[1,2]/(sqrt(sig4[1,1])*sqrt(sig4[2,2]))


## las varianzas
sig5<-matrix(c(4,9,9,25),2,2)
x5<-seq(mu1-2*sig5[1,1],mu1+2*sig5[1,1],by=0.5)
y5<-seq(mu2-2*sig5[2,2],mu2+2*sig5[2,2],by=0.5)
f5<-densidad(x5,y5,mu1,mu2,sig5)
rho5=sig5[1,2]/(sqrt(sig5[1,1])*sqrt(sig5[2,2]))


## las varianzas
sig7<-matrix(c(4,-9,-9,25),2,2)
x7<-seq(mu1-2*sig7[1,1],mu1+2*sig7[1,1],by=0.5)
y7<-seq(mu2-2*sig7[2,2],mu2+2*sig7[2,2],by=0.5)
f7<-densidad(x7,y7,mu1,mu2,sig7)
rho7=sig7[1,2]/(sqrt(sig7[1,1])*sqrt(sig7[2,2]))


## las varianzas
sig8<-matrix(c(4,7,7,25),2,2)
x8<-seq(mu1-2*sig8[1,1],mu1+2*sig8[1,1],by=0.5)
y8<-seq(mu2-2*sig8[2,2],mu2+2*sig8[2,2],by=0.5)
f8<-densidad(x8,y8,mu1,mu2,sig8)
rho8=sig8[1,2]/(sqrt(sig8[1,1])*sqrt(sig8[2,2]))

## las varianzas
sig<-matrix(c(4,-7,-7,25),2,2)
x<-seq(mu1-2*sig[1,1],mu1+2*sig[1,1],by=0.5)
y<-seq(mu2-2*sig[2,2],mu2+2*sig[2,2],by=0.5)
f<-densidad(x,y,mu1,mu2,sig)
rho=sig[1,2]/(sqrt(sig[1,1])*sqrt(sig[2,2]))


## las varianzas
sig6<-matrix(c(4,0,0,25),2,2)
x6<-seq(mu1-2*sig6[1,1],mu1+2*sig6[1,1],by=0.5)
y6<-seq(mu2-2*sig6[2,2],mu2+2*sig6[2,2],by=0.5)
f6<-densidad(x6,y6,mu1,mu2,sig6)
rho6=sig6[1,2]/(sqrt(sig6[1,1])*sqrt(sig6[2,2]))


par(mfrow=c(3,3))

contour(x7,y7,f7,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=-0.9")
contour(x,y,f,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=-0.7")
contour(x1,y1,f1,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=-0.4")
contour(x3,y3,f3,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=-0.2")
contour(x6,y6,f6,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=0")
contour(x4,y4,f4,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=0.2")
contour(x2,y2,f2,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=0.4")
contour(x8,y8,f8,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=0.7")
contour(x5,y5,f5,xlim=c(-4.5,4.5),ylim=c(-15,15),main="rho=0.9")

