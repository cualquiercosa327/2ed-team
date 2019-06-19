
binom<-function(n,x,p0){
estimacion<-x/n
estad<-2*(x*log(estimacion/p0)+(n-x)*log((1-estimacion)/(1-p0)))
p.val<-pchisq(estad,1,lower.tail=F)
list(estimacion=estimacion,estadistica=estad,p.val=p.val)
}

binom(30,7,0.5)