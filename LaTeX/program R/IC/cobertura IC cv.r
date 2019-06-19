set.seed(1)

NG<-1000
n<-seq(5,200,1)
alpha<-0.05


sigma<-50



## función que calcula la probabilidad de cobertura real y longitud esperada para un cv poblacional dado para las tres propuestas
cv.fun<-function(cv){
mu<-sigma/cv
cont1<-cont2<-cont3<-longi1<-longi2<-longi3<-rep(NA,length(n))
for(k in 1:length(n)){
aux1<-aux2<-aux3<-rep(NA,NG)
aux.cont1<-aux.cont2<-aux.cont3<-0
for(i in 1:NG){
dato<-rnorm(n[k],mu,sigma)
## intervalo para mu
inf_mu<-t.test(dato)$conf.int[1]
sup_mu<-t.test(dato)$conf.int[2]

## intervalo para sigma
inf_sig<-sqrt((n[k]-1)*var(dato)/qchisq(1-alpha/2,n[k]-1))
sup_sig<-sqrt((n[k]-1)*var(dato)/qchisq(alpha/2,n[k]-1))

## propuesta 1
inf_cv1<-inf_sig/mean(dato)
sup_cv1<-sup_sig/mean(dato)


## propuesta 2
inf_cv2<-sqrt(var(dato))/sup_mu
sup_cv2<-sqrt(var(dato))/inf_mu


## propuesta 3
inf_cv3<-inf_sig/sup_mu
sup_cv3<-sup_sig/inf_mu


if(inf_cv1<cv&&cv<sup_cv1){aux.cont1<-aux.cont1+1}
if(inf_cv2<cv&&cv<sup_cv2){aux.cont2<-aux.cont2+1}
if(inf_cv3<cv&&cv<sup_cv3){aux.cont3<-aux.cont3+1}

aux1[i]<-sup_cv1-inf_cv1
aux2[i]<-sup_cv2-inf_cv2
aux3[i]<-sup_cv3-inf_cv3
}
longi1[k]<-mean(aux1)
longi2[k]<-mean(aux2)
longi3[k]<-mean(aux3)

cont1[k]<-aux.cont1
cont2[k]<-aux.cont2
cont3[k]<-aux.cont3
}
return(cont1,cont2,cont3,longi1,longi2,longi3)
}

cv1<-cv.fun(0.2)

cv2<-cv.fun(0.4)

cv3<-cv.fun(0.6)

cv4<-cv.fun(0.8)


### plotear  las probabilidades de cobertura

par(mfrow=c(4,3))

plot(cv1$cont1/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 1, cv=0.2",ylim=c(0.4,1))
abline(0.95,0)
plot(cv1$cont2/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 2, cv=0.2",ylim=c(0.4,1))
abline(0.95,0)
plot(cv1$cont3/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 3, cv=0.2",ylim=c(0.4,1))
abline(0.95,0)



plot(cv2$cont1/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 1, cv=0.4",ylim=c(0.4,1))
abline(0.95,0)
plot(cv2$cont2/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 2, cv=0.4",ylim=c(0.4,1))
abline(0.95,0)    
plot(cv2$cont3/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 3, cv=0.4",ylim=c(0.4,1))
abline(0.95,0)

plot(cv3$cont1/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 1, cv=0.6",ylim=c(0.4,1))
abline(0.95,0)
plot(cv3$cont2/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 2, cv=0.6",ylim=c(0.4,1))
abline(0.95,0)
plot(cv3$cont3/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 3, cv=0.6",ylim=c(0.4,1))
abline(0.95,0)


plot(cv4$cont1/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 1, cv=0.8",ylim=c(0.4,1))
abline(0.95,0)
plot(cv4$cont2/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 2, cv=0.8",ylim=c(0.4,1))
abline(0.95,0)
plot(cv4$cont3/NG,type="l",ylab="Cobertura real",xlab="n",main="Propuesta 3, cv=0.8",ylim=c(0.4,1))
abline(0.95,0)


### plotear  las longitudes

par(mfrow=c(2,2))

plot(cv1$longi1,type="l",ylab="Longitud estimada",xlab="n",main="cv=0.2",ylim=c(0,0.72),lwd=2)
lines(cv1$longi2,lty=3,lwd=2)
lines(cv1$longi3,lty=2,lwd=2)
legend(50,0.7,c("Propuesta 1", "Propuesta 2", "Propuesta 3"),lty=c(1,3,2),bty="n",lwd=c(2,2,2))


plot(cv2$longi1,type="l",ylab="Longitud estimada",xlab="n",main="cv=0.4",ylim=c(0,3) ,lwd=2)
lines(cv2$longi2,lty=3,lwd=2)
lines(cv2$longi3,lty=2,lwd=2)
legend(50,2.7,c("Propuesta 1", "Propuesta 2", "Propuesta 3"),lty=c(1,3,2),bty="n",lwd=c(2,2,2))


plot(cv3$longi1,type="l",ylab="Longitud estimada",xlab="n",main="cv=0.6",ylim=c(0,6),lwd=2)
lines(cv3$longi2,lty=3,lwd=2)
lines(cv3$longi3,lty=2,lwd=2)
legend(50,5,c("Propuesta 1", "Propuesta 2", "Propuesta 3"),lty=c(1,3,2),bty="n",lwd=c(2,2,2))


plot(cv4$longi1,type="l",ylab="Longitud estimada",xlab="n",main="cv=0.8",ylim=c(-1,5),lwd=2)
lines(cv4$longi2,lty=3,lwd=2)
lines(cv4$longi3,lty=2,lwd=2)
legend(50,5,c("Propuesta 1", "Propuesta 2", "Propuesta 3"),lty=c(1,3,2),bty="n",lwd=c(2,2,2))
