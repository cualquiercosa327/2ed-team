set.seed(1)


fafb<-function(n,alpha){
x<-seq(0,3*n,0.01)
x<-x[-1]
f<-dchisq(x,n)
maxi<-which(f==max(f))    ## el índice de x donde alcanza el máximo (la moda)
ind<-matrix(NA,maxi,2)
integrales<-rep(NA,maxi)

chisq<-function(x){
return(dchisq(x,n))
}

for(i in 1:maxi){
ind[i,1]<-i
aux<-x[-(1:maxi)]^2*f[(maxi+1):length(f)]
y<-which(abs(x[i]^2*f[i]-aux)==min(abs(x[i]^2*f[i]-aux)))+maxi-1
ind[i,2]<-y
integrales[i]<-integrate(chisq,x[i],x[y])$value
}


val<-which(abs(integrales-(1-alpha))==min(abs(integrales-(1-alpha))))
x[ind[val,1]]
x[ind[val,2]]
return(list(a=x[ind[val,1]],b=x[ind[val,2]]))
}


n<-c(10,30,50,100)
sigma<-1
alpha<-0.05
m<-1000 ## número de simulaciones
l1<-l2<-rep(NA,length(n))
### IC es aux1, IC* es aux2

for(i in 1:length(n)){
aux1<-aux2<-rep(NA,m)
val<-fafb(n[i],alpha)
for(j in 1:m){
dato<-rnorm(n[i],mean=0,sd=sigma)

aux1[j]<-sqrt(sum(dato^2)/val$a)-sqrt(sum(dato^2)/val$b)
aux2[j]<-sqrt(sum(dato^2)/qchisq(alpha/2,n[i]))-sqrt(sum(dato^2)/qchisq(1-alpha/2,n[i]))
}
l1[i]<-mean(aux1)
l2[i]<-mean(aux2)
}


plot(l1,ylim=c(0.2,max(l1,l2)),col=2,type="b",xlab="n",ylab="longitud esperada",xaxt="n")
lines(l2,col=4,type="b", pch=2)
axis(1, 1:length(n), n)
legend(3,1,c("IC","IC*"), col=c(2,4), lty=c(1,1),pch=c(1,2))
