## tamaño de las pruebas

set.seed(1234)

p<-c(0.1,0.3,0.5,0.7,0.9)
n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

tama1<-tama2<-tama3<-matrix(NA,length(p),length(n))
for(i in 1:length(p)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-0
for(k in 1:n.sim){
x<-rbinom(1,n[j],p[i])
norm1<-prop.test(x,n[j],p[i],correct=F)$p.value
norm2<-prop.test(x,n[j],p[i],correct=T)$p.value
exact<-binom.test(x,n[j],p[i])$p.value      
if(norm1<alpha){aux1<-aux1+1}
if(norm2<alpha){aux2<-aux2+1}
if(exact<alpha){aux3<-aux3+1}

}
tama1[i,j]<-aux1 ## normal sin corrección
tama2[i,j]<-aux2 ## normal con corrección
tama3[i,j]<-aux3 ## binomial exacto     
}
}
tama1/n.sim
tama2/n.sim
tama3/n.sim   

#--------------------------------
## potencia de las pruebas
#--------------------------------


set.seed(1234)
p0<-0.5
p<-c(.1,0.3,0.7,0.9)

n<-c(5,15,30,50,100)
n.sim<-1000
alpha<-0.05

pote1<-pote2<-pote3<-matrix(NA,length(p),length(n))
for(i in 1:length(p)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-0
for(k in 1:n.sim){
x<-rbinom(1,n[j],p[i])
norm1<-prop.test(x,n[j],p0,correct=F)$p.value
norm2<-prop.test(x,n[j],p0,correct=T)$p.value
exact<-binom.test(x,n[j],p0)$p.value
if(norm1<alpha){aux1<-aux1+1}
if(norm2<alpha){aux2<-aux2+1}
if(exact<alpha){aux3<-aux3+1}
}
pote1[i,j]<-aux1 ## normal sin corrección  
pote2[i,j]<-aux2 ## normal con corrección  
pote3[i,j]<-aux3 ## binomial exacto        
}
}
pote1/n.sim
pote2/n.sim
pote3/n.sim


