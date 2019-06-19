set.seed(1234)
n<-c(10,30,50,100,300,500,1000)
tg<-matrix(NA)
kg<-matrix(NA)

for(i in 1:length(n)){
d<-rgamma(n[i],shape=3,scale=2)
tg[i]<-var(d)/mean(d)
kg[i]<-mean(d)/tg[i]
}

par(mfrow=c(2,1))
plot(tg,type="b",ylab="",xaxt="n",xlab="n",main="Parámetro de escala")
axis(1,1:length(n),n)
abline(h=2)

plot(kg,type="b",ylab="",xaxt="n",xlab="n",main="Parámetro de forma")
axis(1,1:length(n),n)
abline(h=3)