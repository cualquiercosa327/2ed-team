n<-5
p<-0.4
proba<-rep(NA,n+1)
for(i in 1:length(proba)){
proba[i]<-dbinom(i-1,n,p)
}

plot(proba,type="h",xaxt="n",xlab="x",ylab="p(x)")
