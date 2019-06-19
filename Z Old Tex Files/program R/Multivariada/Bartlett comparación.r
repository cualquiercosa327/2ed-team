set.seed(123456)

Fin.Bart<-rep(NA,4)
Fin.NoBart<-rep(NA,4)
Fin.R<-rep(NA,4)

N.sim<-1000
k<-4

n<-c(10,20,50,100)

for(j in 1:length(n)){
cont.Bartlett<-0
cont.NoBart<-0
cont.R<-0
for(i in 1:N.sim){
x1<-rnorm(n[j])
x2<-rnorm(n[j])
x3<-rnorm(n[j])
x4<-rnorm(n[j])
A<-(k*n[j]-k)*log(var(c(x1,x2,x3,x4))*(n[j]*k-1)/(n[j]*k-k))
B<-(log(var(x1))+log(var(x2))+log(var(x3))+log(var(x4)))*(n[j]-1)
Co<-1+(k/(n[j]-1)-1/(k*n[j]-k))/(3*(k-1))
Bart<-(A-B)/Co
if(Bart>qchisq(0.95,k-1)){cont.Bartlett<-cont.Bartlett+1}

E<-n[j]*k*log(var(c(x1,x2,x3,x4))*(n[j]*k-1)/(n[j]*k))-n[j]*(log(var(x1)*(n[j]-1)/n[j])+log(var(x2)*(n[j]-1)/n[j])+log(var(x3)*(n[j]-1)/n[j])+log(var(x4)*(n[j]-1)/n[j]))
if(E>qchisq(0.95,k-1)){cont.NoBart<-cont.NoBart+1}

if(bartlett.test(list(x1,x2,x3,x4))$p.value<0.05){cont.R<-cont.R+1}
}
Fin.Bart[j]<-cont.Bartlett/N.sim
Fin.NoBart[j]<-cont.NoBart/N.sim
Fin.R[j]<-cont.R/N.sim

}

Fin.Bart
Fin.NoBart
Fin.R