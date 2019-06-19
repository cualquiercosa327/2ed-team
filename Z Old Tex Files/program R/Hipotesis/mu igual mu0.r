#----------------------------------------------
#            mu=mu0
#----------------------------------------------

set.seed(1234)

mu<-3 ## mu verdadero
mu0<-seq(mu-1,mu+1,0.05)   # mu0 de la hipótesis nula
sigma<-1
n<-30
alpha<-0.05

tama<-rep(NA,length(mu0))
n.sim<-1000

for(i in 1:length(mu0)){
aux<-0
for(j in 1:n.sim){
datos<-rnorm(n,mu,sigma)
z<-abs((mean(datos)-mu0[i])*sqrt(n)/sigma)
if(z<qnorm(1-alpha/2)){aux<-aux+1}
else{aux<-aux}
}
tama[i]<-aux
}
tama<-tama/n.sim
## Graficar los resultados

plot(tama,ylim=c(0,1),xaxt="n",type="o",xlab="mu0",ylab="Tamaño de la prueba")
axis(1, 1:length(mu0), mu0)
abline(h=0.95)

#----------------------------------------------
#            mu<=mu0
#----------------------------------------------

set.seed(1234)

mu<-3 ## mu verdadero
mu0<-seq(mu,mu+3,0.05)   # mu0 de la hipótesis nula
sigma<-1
n<-30
alpha<-0.05

tama<-rep(NA,length(mu0))
n.sim<-1000

for(i in 1:length(mu0)){
aux<-0
for(j in 1:n.sim){
datos<-rnorm(n,mu,sigma)
z<-(mean(datos)-mu0[i])*sqrt(n)/sigma
if(z<qnorm(1-alpha)){aux<-aux+1}
else{aux<-aux}
}
tama[i]<-aux
}
tama<-tama/n.sim
## Graficar los resultados

plot(tama,ylim=c(0,1),xaxt="n",type="o",xlab="mu0",ylab="Tamaño de la prueba")
axis(1, 1:length(mu0), mu0)
abline(h=0.95)
