set.seed(123)


n<-c(5,10,20,50,100,500,1000)
ng<-1000
theta<-5
pro.Exp<-pro.t<-matrix(NA)
aux.Exp<-aux.t<-0

for(i in 1:length(n)){
for(j in 1:ng){
x<-rgamma(n[i],shape=1,scale=theta)
if(t.test(x)$conf.int[1]>theta||t.test(x)$conf.int[2]<theta){aux.t<-aux.t}
if(t.test(x)$conf.int[1]<=theta&&t.test(x)$conf.int[2]>=theta){aux.t<-aux.t+1}
Exp.inf<-sum(x)/qgamma(0.975,shape=n[i],scale=1)
Exp.sup<-sum(x)/qgamma(0.025,shape=n[i],scale=1)
if(Exp.inf>theta||Exp.sup<theta){aux.Exp<-aux.Exp}
if(Exp.inf<=theta&&Exp.sup>=theta){aux.Exp<-aux.Exp+1}
}
pro.Exp[i]<-aux.Exp/ng
pro.t[i]<-aux.t/ng
aux.Exp<-aux.t<-0
}

plot(pro.Exp,type="b", col=4,ylim=c(0.85,1),xaxt="n",ylab="Probabilida de cobertura",xlab="n")
lines(pro.t,col="red",type="b", pch=2)
abline(h=0.95)
axis(1, 1:length(n), n)
legend(50,1,c("IC","Intervalo t"), col=c(4,2), lty=c(1,1),pch=c(1,2))


