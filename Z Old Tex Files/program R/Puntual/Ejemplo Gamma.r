set.seed(12345)
x<-rgamma(127,shape=8,scale=150000)

# Graficar la función de densidad Gamma
va<-var(x)*(n-1)/n
k<-(mean(x))^2/va
k
the<-va/mean(x)
the

gama.densidad<-function(x){
fx<-dgamma(x,shape=k,scale=the)
}

hist(x,breaks=20,freq=F)
curve(gama.densidad(x),add=T)


