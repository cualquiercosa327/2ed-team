## tamaño de las pruebas

set.seed(1234)

p<-c(0.1,0.3,0.5,0.7,0.9)
n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

tama1<-tama2<-tama3<-tama4<-matrix(NA,length(p),length(n))
for(i in 1:length(p)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-aux4<-0
for(k in 1:n.sim){
x<-rbinom(1,n[j],p[i])
y<-rbinom(1,n[j],p[i])
if(x==0&y==0){
aux1<-aux1
aux2<-aux2
}
if(x==n[j]&y==n[j]){
aux1<-aux1
aux2<-aux2
}
if(0<x&x<n[j]&0<y&y<n[j]){
norm1<-prop.test(c(x,y),c(n[j],n[j]),correct=F)$p.value
norm2<-prop.test(c(x,y),c(n[j],n[j]),correct=T)$p.value
if(norm1<alpha){aux1<-aux1+1}
if(norm2<alpha){aux2<-aux2+1}
}
dato<-matrix(c(x,n[j]-x,y,n[j]-y),2,2)
fisher<-fisher.test(dato)$p.value
chi<-chisq.test(dato)$p.value

if(fisher<alpha){aux3<-aux3+1}
if(chi<alpha){aux4<-aux4+1}

}
tama1[i,j]<-aux1 ## normal sin corrección
tama2[i,j]<-aux2 ## normal con corrección
tama3[i,j]<-aux3 ## fisher
tama4[i,j]<-aux4 ## chi cuadrado
}
}
tama1/n.sim
tama2/n.sim
tama3/n.sim
tama4/n.sim

#--------------------------------
## potencia de las pruebas
#--------------------------------


set.seed(1234)
p0<-0.1
p<-c(.2,0.4,0.6,0.8)

n<-c(5,15,30,50,100)
n.sim<-10000
alpha<-0.05

pote1<-pote2<-pote3<-pote4<-matrix(NA,length(p),length(n))
for(i in 1:length(p)){
for(j in 1:length(n)){
aux1<-aux2<-aux3<-aux4<-0
for(k in 1:n.sim){
x<-rbinom(1,n[j],p0)
y<-rbinom(1,n[j],p0+p[i])
if(x==0&y==0){
aux1<-aux1
aux2<-aux2
}
if(x==n[j]&y==n[j]){
aux1<-aux1
aux2<-aux2
}
if(0<x&x<n[j]&0<y&y<n[j]){
norm1<-prop.test(c(x,y),c(n[j],n[j]),correct=F)$p.value
norm2<-prop.test(c(x,y),c(n[j],n[j]),correct=T)$p.value
if(norm1<alpha){aux1<-aux1+1}
if(norm2<alpha){aux2<-aux2+1}
}
dato<-matrix(c(x,n[j]-x,y,n[j]-y),2,2)
fisher<-fisher.test(dato)$p.value
chi<-chisq.test(dato)$p.value

if(fisher<alpha){aux3<-aux3+1}
if(chi<alpha){aux4<-aux4+1}
}
pote1[i,j]<-aux1 ## normal sin corrección  
pote2[i,j]<-aux2 ## normal con corrección  
pote3[i,j]<-aux3 ## fisher
pote4[i,j]<-aux4 ## chi cuadrado      
}
}
pote1/n.sim
pote2/n.sim
pote3/n.sim
pote4/n.sim


