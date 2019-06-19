## tamaño de las pruebas

set.seed(1234)

lambda<-c(2,5,15,25)
n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

tama1<-tama2<-matrix(NA,length(lambda),length(n))
for(i in 1:length(lambda)){
for(j in 1:length(n)){
aux1<-aux2<-0
for(k in 1:n.sim){
muestra<-rpois(n[j],lambda[i])
ba<-mean(muestra)
LRT<-2*(n[j]*ba*log(ba/lambda[i])+n[j]*(lambda[i]-ba))
if(LRT>qchisq(1-alpha,1)){aux1<-aux1+1}
if(ba>(qpois(1-alpha/2,n[j]*lambda[i])/n[j])|ba<(qpois(alpha/2,n[j]*lambda[i])/n[j])){
aux2<-aux2+1}
}
tama1[i,j]<-aux1 ## Razón de verosimilitud
tama2[i,j]<-aux2 ## Exacto
}
}
tama1/n.sim
tama2/n.sim

#--------------------------------
## potencia de las pruebas
#--------------------------------

lambda0<-20
lambda<-lambda0+seq(-7,7,2)

n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

pote1<-pote2<-matrix(NA,length(lambda),length(n))
for(i in 1:length(lambda)){
for(j in 1:length(n)){
aux1<-aux2<-0
for(k in 1:n.sim){
muestra<-rpois(n[j],lambda[i])
ba<-mean(muestra)
LRT<-2*(n[j]*ba*log(ba/lambda0)+n[j]*(lambda0-ba))
if(LRT>qchisq(1-alpha,1)){aux1<-aux1+1}
if(ba>(qpois(1-alpha/2,n[j]*lambda0)/n[j])|ba<(qpois(alpha/2,n[j]*lambda0)/n[j])){
aux2<-aux2+1}
}
pote1[i,j]<-aux1 ## Razón de verosimilitud
pote2[i,j]<-aux2 ## Exacto
}
}
pote1/n.sim
pote2/n.sim


