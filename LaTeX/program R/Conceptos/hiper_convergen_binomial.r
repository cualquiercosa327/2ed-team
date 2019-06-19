Hg<-function(x,n,R,N){
x<-c(max(0,n-N+R):min(R,n))
res<-rep(NA,length(x))
for(i in 1:length(x)){
res[i]<-choose(R,x[i])*choose(N-R,n-x[i])/choose(N,n)}
return(res)
}

par(mfrow=c(2,2))

plot(Hg(x,30,20,50),type="h",main="Hg(30,20,50)",xlab="x",ylab="f(x)")
lines(dbinom(x,n,20/50),col=2)

plot(Hg(x,30,50,80),type="h",main="Hg(30,50,80)",xlab="x",ylab="f(x)")
lines(dbinom(x,n,50/80),col=2)

plot(Hg(x,30,100,200),type="h",main="Hg(30,100,200)",xlab="x",ylab="f(x)")
lines(dbinom(x,n,100/200),col=2)

plot(Hg(x,30,300,1000),type="h",main="Hg(30,300,1000)",xlab="x",ylab="f(x)")
lines(dbinom(x,n,300/1000),col=2)


