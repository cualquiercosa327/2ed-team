box.cox<-function(x){
if(prod(x<=0)!=0){
y<-x-min(x)+1e-10
}
if(prod(x<=0)==0){y<-x}
n<-length(y)

      L<-function(y,lambda){
      if(lambda==0){
      y.lam<-log(y) }
      if(lambda!=0){
      y.lam<-(y^lambda-1)/lambda  }

      mu<-mean(y.lam)
      sigma2<-var(y.lam)*(n-1)/n
      L.lam<-(lambda-1)*sum(log(y))-log(sigma2)*n/2

      list("L.lam"=L.lam,"xtrans"=y.lam)
      }

lam<-seq(0,100,0.01)
L.val<-rep(NA,length(lam))
for(i in 1:length(lam)){
L.val[i]<-L(y,lam[i])$L.lam
}
lam.MV=lam[which(L.val==max(L.val))]
datos.tranf<-L(y,lam.MV)$xtrans
list("lambda"=lam.MV,"xtrans"=datos.tranf)
}

set.seed(12345)
a<-rgamma(50,2,5)
transf<-box.cox(a)
transf$lambda
datos.trans<-transf$xtrans
shapiro.test(a)
shapiro.test(datos.trans)
ks.test(a,"pnorm",mean(a),sd(a))
ks.test(datos.trans,"pnorm",mean(datos.trans),sd(datos.trans))

a<-runif(50,-1,5)

