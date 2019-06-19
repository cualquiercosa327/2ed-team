
### bondad del método Delta
### poblaciones exponencial
### comparar el tamaño muestral

set.seed(123)

n<-c(5,10,30,50,100,500)
theta1<-1
theta2<-5

## g la función p(1-p)^3

g1<-1-exp(-1/theta1) ## Pr(X<1)
g2<-exp(-1/theta1)-exp(-2/theta1) ## Pr(1<X<2)

f1<-1-exp(-1/theta2) ## Pr(X<1)
f2<-exp(-1/theta2)-exp(-2/theta2) ## Pr(1<X<2)


## para guardar los promedios de las estimaciones
res.1<-res.2<-rep(NA,length(n))
res.f1<-res.f2<-rep(NA,length(n))
## número de simulaciones
NG<-1000

for(i in 1:length(n)){
## simular muestrales de la Bernoulli
m1<-mean(rexp(n[i],1/theta1))
m2<-mean(rexp(n[i],1/theta2))
g.t1<-1-exp(-1/m1)
g.t2<-exp(-1/m1)-exp(-2/m1)


f.t1<-1-exp(-1/m2)
f.t2<-exp(-1/m2)-exp(-2/m2)

for(g in 2:NG){
m1<-mean(rexp(n[i],1/theta1))
m2<-mean(rexp(n[i],1/theta2))

g.t1<-1-exp(-1/m1)+g.t1
g.t2<-exp(-1/m1)-exp(-2/m1)+g.t2

f.t1<-1-exp(-1/m2)+f.t1
f.t2<-exp(-1/m2)-exp(-2/m2)+f.t2



}
g.t1<-g.t1/NG
g.t2<-g.t2/NG

f.t1<-f.t1/NG
f.t2<-f.t2/NG

res.1[i]<-g.t1
res.2[i]<-g.t2

res.f1[i]<-f.t1
res.f2[i]<-f.t2
}


par(mfrow=c(1,2))

mini<-min(res.1-rep(g1,length(n)),res.2-rep(g2,length(n)))
maxi<-max(res.1-rep(g1,length(n)),res.2-rep(g2,length(n)))


plot(res.1-rep(g1,length(n)),type="b",ylim=c(-0.05,0.05),ylab="",xaxt="n",xlab="n",main="theta=1")
lines(res.2-rep(g2,length(n)),type="b", pch=4)

abline(h=0)
axis(1,1:length(n),n)
legend(4,-0.03,c("  T_1","  T_2"), pch=c(1,4),bty="n")



minif<-min(res.f1-rep(f1,length(n)),res.f2-rep(f2,length(n)))
maxif<-max(res.f1-rep(f1,length(n)),res.f2-rep(f2,length(n)))


plot(res.f1-rep(f1,length(n)),type="b",ylim=c(-0.05,0.05),ylab="",xaxt="n",xlab="n",main="theta=5")
lines(res.f2-rep(f2,length(n)),type="b", pch=4)

abline(h=0)
axis(1,1:length(n),n)
legend(4,-0.03,c("  T_1","  T_2"), pch=c(1,4),bty="n")

