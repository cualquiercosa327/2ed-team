mardia<-function(x){
x<-data.frame(x)
n<-dim(x)[1]
p<-dim(x)[2]
bar<-mean(x)
S<-matrix(var(x)*(n-1)/n,2,2)
b1.ma<-matrix(NA,n,n)
b2.ma<-rep(NA,n)
for(i in 1:n){
for(j in 1:n){
b1.ma[i,j]<-(sum((x[i,]-bar)*(solve(S)%*%t(x[j,]-bar))))^3
}
b2.ma[i]<-(sum((x[i,]-bar)*(solve(S)%*%t(x[i,]-bar))))^2
}
b1<-sum(b1.ma)/(n^2)
b2<-sum(b2.ma)/n
B1<-n*b1/6
B2<-(b2-p*(p+2))/(sqrt(8*p*(p+2)/n))
v<-p*(p+1)*(p+2)/6
p.val1<-pchisq(B1,v,lower.tail=F)
p.val2<-2*pnorm(abs(B2),lower.tail=F)
list("B1"=B1,"pval1"=p.val1,"B2"=B2,"pval2"=p.val2)

}

ante<-c(230,245,220,250, 260,250,220,300,310,290,260,240,210,220,
250,245,274,230,285,275)
desp<-c(210,230,215,220,240,220,210,260,280,270,230,235,200,200,
210,230,250,210,260,230)
x<-data.frame(cbind(ante,desp))

mardia(x)