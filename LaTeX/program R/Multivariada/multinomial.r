#--------------------------------
### p=p_0 en una muestra
#--------------------------------

multi<-function(x,p0){
if(length(x)!=length(p0))
stop("X y P0 deben tener el mismo tamaño")
r<-length(x)
est<-x/sum(x)
estad<-2*sum(x*log(est/p0))
p<-pchisq(estad,r-1,lower.tail = F)
list(estima=est,estadistica=estad,p.valor=p)
}

x<-c(810,654,972)
p_0<-c(0.43,0.25,0.32)

multi(x,p_0)

#--------------------------------
## igualdad en dos muestras
#--------------------------------

multi_2_muestra<-function(x,y){
if(length(x)!=length(y))
stop("X y Y deben tener el mismo tamaño")
r<-length(x)
est.X<-x/sum(x)
est.Y<-y/sum(y)
l1<-sum(x*log(x/sum(x)))+sum(y*log(y/sum(y)))
l2<-sum((x+y)*log((x+y)/sum(x+y)))
estad<-2*(l1-l2)
p<-pchisq(estad,r-1,lower.tail = F)
list(estima.X=est.X,estima.Y=est.Y,estadistica=estad,p.valor=p)
}

hombre<-c(192,140,17)
mujer<-c(127,90,20)
multi_2_muestra(hombre,mujer)

#--------------------------------
## igualdad en k muestras
#--------------------------------

multi_k_muestra<-function(x){                                    
# columna j de x debe corresponder a los conteos en la muestra j 
r<-dim(x)[1]                                                     
k<-dim(x)[2]                                                     
est<-matrix(NA,r,k)                                              
for(j in 1:k){                                                   
est[,j]<-x[,j]/colSums(x)[j]} ## estimación MV                   
p_0<-rowSums(x)/(sum(x)) ## estimación MV bajo H0                
l1<-sum(x*log(est))                                              
l2<-sum(rowSums(x)*log(p_0))                                     
estad<-2*(l1-l2)                                                 
p<-pchisq(estad,(r-1)*(k-1),lower.tail = F)                      
list(est=est,estadistica=estad,p.valor=p)                        
}        

x<-matrix(c(95,120,35,6,53,97,45,13,33,79,19,3),4,3) 
multi_k_muestra(x)