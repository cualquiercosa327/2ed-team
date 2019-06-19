set.seed(1)

 n<-c(2,10,30, 50,100, 300, 1000,5000)
 #Aumenta el tamaño de muestra

 var1<-rep(NA,length(n))
 var2<-rep(NA,length(n))

 for(k in 1:length(n))
  {
  data<-rnorm(n[k],5,9)
  var1[k]<-var(data)
  var2[k]<-(n[k]-1)*var(data)/n[k]
  }

  plot(var1,type="b", col=4,ylim=c(min(var2),130),xlab="Tamaño de muestra", ylab="Estimación de la varianza", xaxt="n")
  lines(var2,type="b", col=2, pch=2)
  abline(h=81)
  axis(1, 1:length(n), n)
  legend(3,120,c("Insesgado","Sesgado"), col=c(4,2), lty=c(1,1),pch=c(1,2))

