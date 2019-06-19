#--------------------------------
## igualdad en dos muestras Poisson
#--------------------------------

pois_2<-function(x,y){
nx<-length(x)
ny<-length(y)
est.X<-mean(x)
est.Y<-mean(y)
l1<-sum(x)*log(est.X)+sum(y)*log(est.Y)
l2<-(sum(x)+sum(y))*log((sum(x)+sum(y))/(nx+ny))
estad<-2*(l1-l2)
p<-pchisq(estad,1,lower.tail = F)
list(estima.X=est.X,estima.Y=est.Y,estadistica=estad,p.valor=p)
}

Durante<-c(36, 31 ,28, 41 ,35 ,52, 25, 31, 32, 34)
Despues<-c(31 ,41 ,29 ,20, 29, 35 ,31, 29, 33 ,30)
pois_2(Durante,Despues)

