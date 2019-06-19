
mu2<-function(a11,a12,a22,z,mu1){
mu2.1<-sqrt((z-a11*(mu1^2))/a22+((a12*mu1)^2)/(a22^2))-a12*mu1/a22
mu2.2<--sqrt((z-a11*(mu1^2))/a22+((a12*mu1)^2)/(a22^2))-a12*mu1/a22
return(mu2.1,mu2.2)
}

################################# 
## Región de confianza S1
################################# 

n<-10
z<-qchisq(0.95,2)/n

par(mfrow=c(2,2))
mu1<-seq(-3,3,0.00005)
sig11<-1
sig12<-0
sig22<-1
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),type="l",ylab="mu2",main="(a)")
lines(mu1,res$mu2.2)


mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-0
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(b)")
lines(mu1,res$mu2.2)

mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(c)")
lines(mu1,res$mu2.2)

mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<--2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(d)")
lines(mu1,res$mu2.2)



################################# 
## Región de confianza S2
################################# 

n<-10
z<-qchisq(0.05,2)/n

par(mfrow=c(2,2))
mu1<-seq(-3,3,0.000005)
sig11<-1
sig12<-0
sig22<-1
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-.2,.2),ylim=c(-.2,.2),type="l",ylab="mu2",main="(a)")
lines(mu1,res$mu2.2)


mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-0
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-.3,.3),ylim=c(-.3,.3),type="l",ylab="mu2",main="(b)")
lines(mu1,res$mu2.2)

mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-.3,.3),ylim=c(-.3,.3),type="l",ylab="mu2",main="(c)")
lines(mu1,res$mu2.2)

mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<--2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-.3,.3),ylim=c(-.3,.3),type="l",ylab="mu2",main="(d)")
lines(mu1,res$mu2.2)




################################# 
## Región de confianza S3
################################# 

n<-10
z1<-qchisq(0.025,2)/n
z2<-qchisq(0.975,2)/n

par(mfrow=c(2,2))
mu1<-seq(-1,1,0.000005)
sig11<-1
sig12<-0
sig22<-1
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res1<-mu2(a11,a12,a22,z1,mu1)
res2<-mu2(a11,a12,a22,z2,mu1)
plot(mu1,res1$mu2.1,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),type="l",ylab="mu2",main="(a)")
lines(mu1,res1$mu2.2)
lines(mu1,res2$mu2.2)
lines(mu1,res2$mu2.1)


mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-0
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res1<-mu2(a11,a12,a22,z1,mu1)
res2<-mu2(a11,a12,a22,z2,mu1)
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res1$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(b)")
lines(mu1,res1$mu2.2)
lines(mu1,res2$mu2.2)
lines(mu1,res2$mu2.1)

mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res1<-mu2(a11,a12,a22,z1,mu1)
res2<-mu2(a11,a12,a22,z2,mu1)
plot(mu1,res1$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(c)")
lines(mu1,res1$mu2.2)
lines(mu1,res2$mu2.2)
lines(mu1,res2$mu2.1)



mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<--2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res1<-mu2(a11,a12,a22,z1,mu1)
res2<-mu2(a11,a12,a22,z2,mu1)
plot(mu1,res1$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2",main="(d)")
lines(mu1,res1$mu2.2)
lines(mu1,res2$mu2.2)
lines(mu1,res2$mu2.1)



################################# 
## Región de confianza S1X...
################################# 
mu2<-function(a11,a12,a22,z,mu1){
mu2.1<-sqrt((z-a11*(mu1^2))/a22+((a12*mu1)^2)/(a22^2))-a12*mu1/a22
mu2.2<--sqrt((z-a11*(mu1^2))/a22+((a12*mu1)^2)/(a22^2))-a12*mu1/a22
return(mu2.1,mu2.2)
}


n<-10
z<-qchisq(0.95,2)/n


mu1<-seq(-2,2,0.00005)
sig11<-4
sig12<-2
sig22<-2
Sig<-matrix(c(sig11,sig12,sig12,sig22),2,2)
a11<-solve(Sig)[1,1]
a12<-solve(Sig)[1,2]
a22<-solve(Sig)[2,2]
res<-mu2(a11,a12,a22,z,mu1)
plot(mu1,res$mu2.1,xlim=c(-2,2),ylim=c(-2,2),type="l",ylab="mu2")
lines(mu1,res$mu2.2)


Intmu1<-qnorm(0.975)*sqrt(sig11)/sqrt(n)
Intmu2<-qnorm(0.975)*sqrt(sig22)/sqrt(n)
abline(h=Intmu2)
abline(h=-Intmu2)
abline(v=Intmu1)
abline(v=-Intmu1)