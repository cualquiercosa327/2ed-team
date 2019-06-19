
### bondad del método Delta
### poblaciones exponencial
### comparar el tamaño muestral

set.seed(123)

n<-c(5,10,30,50,100,500)
theta1<-0.7
theta2<-0.3

## g la función p(1-p)^3

g1<-theta1*(1-theta1) ## varianza pobalcional
g2<-4*theta1*(1-theta1)^3 ## probabilidad de un exito en 4 ensayos
g3<-1-(1-theta1)^4    ## probabilidad de que el numero de exito en 4 ensayos sea mayor que 1

f1<-theta2*(1-theta2) ## varianza pobalcional
f2<-4*theta2*(1-theta2)^3 ## probabilidad de un exito en 4 ensayos
f3<-1-(1-theta2)^4    ## probabilidad de que el numero de exito en 4 ensayos sea mayor que 1

## para guardar los promedios de las estimaciones
res.1<-res.2<-res.3<-rep(NA,length(n))
res.f1<-res.f2<-res.f3<-rep(NA,length(n))
## número de simulaciones
NG<-1000

for(i in 1:length(n)){
## simular muestrales de la Bernoulli
m1<-mean(rbinom(n[i],1,theta1))
m2<-mean(rbinom(n[i],1,theta2))
g.t1<-m1*(1-m1)
g.t2<-4*m1*(1-m1)^3
g.t3<-1-(1-m1)^4

f.t1<-m2*(1-m2)
f.t2<-4*m2*(1-m2)^3
f.t3<-1-(1-m2)^4

for(g in 2:NG){
m1<-mean(rbinom(n[i],1,theta1))
m2<-mean(rbinom(n[i],1,theta2))

g.t1<-m1*(1-m1)+g.t1
g.t2<-4*m1*(1-m1)^3+g.t2
g.t3<-1-(1-m1)^4+g.t3

f.t1<-m2*(1-m2)+f.t1
f.t2<-4*m2*(1-m2)^3+f.t2
f.t3<-1-(1-m2)^4+f.t3


}
g.t1<-g.t1/NG
g.t2<-g.t2/NG
g.t3<-g.t3/NG

f.t1<-f.t1/NG
f.t2<-f.t2/NG
f.t3<-f.t3/NG

res.1[i]<-g.t1
res.2[i]<-g.t2
res.3[i]<-g.t3

res.f1[i]<-f.t1
res.f2[i]<-f.t2
res.f3[i]<-f.t3
}


par(mfrow=c(1,2))


minif<-min(res.f1-rep(f1,length(n)),res.f2-rep(f2,length(n)),res.f3-rep(f3,length(n)))
maxif<-max(res.f1-rep(f1,length(n)),res.f2-rep(f2,length(n)),res.f3-rep(f3,length(n)))


plot(res.f1-rep(f1,length(n)),type="b",ylim=c(-0.12,0.05),ylab="",xaxt="n",xlab="n",main="p=0.3")
lines(res.f2-rep(f2,length(n)),type="b", pch=4)
lines(res.f3-rep(f3,length(n)),type="b", pch=2)
abline(h=0)
axis(1,1:length(n),n)
legend(4,-0.03,c("  T_1","  T_2","  T_3"), pch=c(1,4,2),bty="n")

mini<-min(res.1-rep(g1,length(n)),res.2-rep(g2,length(n)),res.3-rep(g3,length(n)))
maxi<-max(res.1-rep(g1,length(n)),res.2-rep(g2,length(n)),res.3-rep(g3,length(n)))


plot(res.1-rep(g1,length(n)),type="b",ylim=c(-0.12,0.05),ylab="",xaxt="n",xlab="n",main="p=0.7")
lines(res.2-rep(g2,length(n)),type="b", pch=4)
lines(res.3-rep(g3,length(n)),type="b", pch=2)
abline(h=0)
axis(1,1:length(n),n)
legend(4,-0.03,c("  T_1","  T_2","  T_3"), pch=c(1,4,2),bty="n")


