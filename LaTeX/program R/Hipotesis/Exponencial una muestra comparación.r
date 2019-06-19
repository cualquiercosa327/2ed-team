## tamaño de las pruebas

set.seed(1234)

theta<-c(2,5,15,25)
n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

tama1<-tama2<-tama3<-matrix(NA,length(theta),length(n))
for(i in 1:length(theta)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-0
for(k in 1:n.sim){
muestra<-rexp(n[j],1/theta[i])
ba<-mean(muestra)
LRT<-2*n[j]*(ba/theta[i]-log(ba/theta[i])-1)
if(LRT>qchisq(1-alpha,1)){aux1<-aux1+1}
if(theta[i]<n[j]*ba/qgamma(1-alpha/2,n[j],1)|theta[i]>n[j]*ba/qgamma(alpha/2,n[j],1)){
aux2<-aux2+1}
if(theta[i]<sqrt(n[j])*ba/(qnorm(1-alpha/2)+sqrt(n[j]))|theta[i]>sqrt(n[j])*ba/(-qnorm(1-alpha/2)+sqrt(n[j]))){
aux3<-aux3+1}
}
tama1[i,j]<-aux1 ## Razón de verosimilitud
tama2[i,j]<-aux2 ## Exacto gamma
tama3[i,j]<-aux3 ## TLC normal
}
}
tama1/n.sim
tama2/n.sim
tama3/n.sim

#--------------------------------
## potencia de las pruebas
#--------------------------------
set.seed(1234)

theta0<-20
theta<-theta0+seq(-7,7,2)

n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

pote1<-pote2<-pote3<-matrix(NA,length(theta),length(n))
for(i in 1:length(theta)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-0
for(k in 1:n.sim){
muestra<-rexp(n[j],1/theta[i])
ba<-mean(muestra)
LRT<-2*n[j]*(ba/theta0-log(ba/theta0)-1)
if(LRT>qchisq(1-alpha,1)){aux1<-aux1+1}
if(theta0<n[j]*ba/qgamma(1-alpha/2,n[j],1)|theta0>n[j]*ba/qgamma(alpha/2,n[j],1)){
aux2<-aux2+1}
if(theta0<sqrt(n[j])*ba/(qnorm(1-alpha/2)+sqrt(n[j]))|theta0>sqrt(n[j])*ba/(-qnorm(1-alpha/2)+sqrt(n[j]))){
aux3<-aux3+1}
}
pote1[i,j]<-aux1 ## Razón de verosimilitud
pote2[i,j]<-aux2 ## Exacto
pote3[i,j]<-aux3 ## TLC
}
}
pote1/n.sim
pote2/n.sim      

##------- ##------- ##------- ##------- ##------- 
##------- Graficar las tres funciones de potencia
##------- ##------- ##------- ##------- ##------- 
set.seed(1234)           
alpha<-0.05
theta0<-20
theta<-theta0+seq(-7,7,2)
n<-30
n.sim<-10000

## Potencia de la prueba exacta gamma
pote_gamma<-function(theta,theta0,n,alpha){
1-pgamma(theta0*qgamma(1-alpha/2,n,1),n,1/theta)+pgamma(theta0*qgamma(alpha/2,n,1),n,1/theta)   
}
# Potencia de la prueba asintótica normal
pote_norm<-function(theta,theta0,n,alpha){
1-pnorm((theta0/theta)*(qnorm(1-alpha/2)+sqrt(n))-sqrt(n))+pnorm((theta0/theta)*(-qnorm(1-alpha/2)+sqrt(n))-sqrt(n))
}
## Potencia de la prueba asintótica chi cuadrado
pote_chi<-function(theta,theta0,n,alpha){
aux<-0
for(k in 1:n.sim){
muestra<-rexp(n,1/theta)
ba<-mean(muestra)
LRT<-2*n*(ba/theta0-log(ba/theta0)-1)
if(LRT>qchisq(1-alpha,1)){aux<-aux+1}
}
aux/n.sim
}
## 

pote_chis<-pote_gammas<-pote_norms<-matrix(NA)
theta<-seq(5,50,0.1)
for(i in 1:length(theta)){
pote_chis[i]<-pote_chi(theta[i],theta0,n,alpha)
pote_gammas[i]<-pote_gamma(theta[i],theta0,n,alpha)
pote_norms[i]<-pote_norm(theta[i],theta0,n,alpha)
}


plot(pote_chis,type="l",xaxt="n",xlab="theta",ylab="Potencia")
axis(1,1:length(theta),theta)
lines(pote_norms,lty=2)
lines(pote_gammas,lty=3)
legend(220,0.3,c("Prueba exacta", "Prueba asintótica normal", "Prueba asintótica chi2"),lty=c(3,2,1),bty="n")


