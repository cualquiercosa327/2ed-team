####
####
#### encontrar a y b con f(a)=f(b)
####
####


alpha<-0.05  
n<-10
x<-seq(0,100,0.01)
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
y<-which(abs(f[i]-f[maxi:length(f)])==min(abs(f[i]-f[maxi:length(f)])))+maxi-1
ind[i,2]<-y
integrales[i]<-integrate(chisq,x[i],x[y])$value
}

val<-which(abs(integrales-(1-alpha))==min(abs(integrales-(1-alpha))))
x[ind[val,1]]
x[ind[val,2]]

####
####
#### encontrar a y b con a^2f(a)=b^2f(b)
####
####

n<-10
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

####
####
### comparación del intervalo más corto y el intervalo popular de sigma^2
####
####

n<-c(10,30,50,100)
sigma2<-1
alpha<-0.05

fafb<-function(n){
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


n1<-n2<-rep(NA,length(n))
for(k in 1:length(n)){
val<-fafb(n[k])
n1[k]<-n[k]*sigma2*((1/val$a)-(1/val$b))
n2[k]<-n[k]*sigma2*((1/qchisq(alpha/2,n[k]))-(1/qchisq(1-alpha/2,n[k])))
}
plot(n2,type="b",xlab="n",col=4, ylab="Longitud esperada", xaxt="n")
lines(n1,type="b", col=2, pch=2)
axis(1, 1:length(n), n)
legend(2.6,2.2,c("IC*","IC"), col=c(4,2), lty=c(1,1),pch=c(1,2),bty="n")

