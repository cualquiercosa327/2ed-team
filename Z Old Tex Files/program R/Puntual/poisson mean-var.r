set.seed(1234)

n<-c(10,30,50,100,300,500,1000)
mp<-matrix(NA)
vp<-matrix(NA)

for(i in 1:length(n)){
a<-rpois(n[i],5)
mp[i]<-mean(a)
vp[i]<-var(a)
}

#####################
mb<-matrix(NA)
vb<-matrix(NA)

for(i in 1:length(n)){
b<-rbinom(n[i],20,5/20)
mb[i]<-mean(b)
vb[i]<-var(b)
}

par(mfrow=c(2,1))

plot(mp,type="b",ylim=c(0,7),ylab="",xaxt="n",xlab="n",main="Poisson(5)")
lines(vp,type="b", pch=4)
axis(1,1:length(n),n)
legend("bottomright",c("Promedio","Varianza"), pch=c(1,4),bty="n")


plot(mb,type="b",ylim=c(0,7),ylab="",xaxt="n",xlab="n",main="Binomial(20,0.25)")
lines(vb,type="b", pch=4)
axis(1,1:length(n),n)
legend("bottomright",c("Promedio","Varianza"), pch=c(1,4),bty="n")


