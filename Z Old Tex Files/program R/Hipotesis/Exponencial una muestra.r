    
    exp_1<-function(x,theta0,type = c("gamma","normal", "chi")){
    
    n<-length(x)
    est<-mean(x)
    
    if(type=="gamma"){
      prueba<-"Prueba exacta gamma"
      if(sum(x)>qgamma(0.5,n,1/theta0)){
      p<-2*pgamma(sum(x),n,1/theta0,lower.tail=F)        }
      else{p<-2*pgamma(sum(x),n,1/theta0)   }
    }
    
    if(type=="normal"){
      prueba<-"Prueba asintótica normal"
      v<-sqrt(n)*(est-theta0)/theta0
      if(v<0){
      p<-2*pnorm(v)        }
      else{p<-2*pnorm(v,lower.tail=F)   }
    }
    
    if(type=="chi"){
      prueba<-"Prueba asintótica chi cuadrado"
      lambda<-2*n*((est/theta0)-log(est/theta0)-1)
      p<-pchisq(lambda,1,lower.tail=F)
    }
    
    list(tipo=prueba,estimación=est,p.valor=p)
    }
    
    tiempo<-c(0.13, 0.06, 0.50, 0.41, 1.44, 0.60, 0.22, 1.08, 0.78, 0.92, 2.73, 0.83, 0.19, 0.21, 1.75, 0.79, 0.02, 0.05, 2.30, 1.03)
    exp_1(tiempo,0.5,type="gamma")
    exp_1(tiempo,0.5,type="normal")
    exp_1(tiempo,0.5,type="chi")
    