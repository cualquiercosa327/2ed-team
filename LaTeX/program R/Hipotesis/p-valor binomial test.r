n<-30
p0<-0.7
x<-10

 # con mayor igual
pval<-function(x,n,p0){
if(x>(n*p0)){
p.val<-1-pbinom(x-1,n,p0)+pbinom(2*n*p0-x,n,p0)
}
if(x<(n*p0)){
p.val<-1-pbinom(ceiling(2*n*p0-x)-1,n,p0)+pbinom(x,n,p0)}
if(x==(n*p0)){
p.val<-1}
p.val
}

 # con mayor
pval1<-function(x,n,p0){
if(x>(n*p0)){
p.val<-1-pbinom(x,n,p0)+pbinom(floor(2*n*p0-x),n,p0)
}
if(x<(n*p0)){
p.val<-1-pbinom(2*n*p0-x,n,p0)+pbinom(x-1,n,p0)}
if(x==(n*p0)){
p.val<-1}
p.val
}


binom.test(x,n,p0)
pval(x,n,p0)
pval1(x,n,p0)

