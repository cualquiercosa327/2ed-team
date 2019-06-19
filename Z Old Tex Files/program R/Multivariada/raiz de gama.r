p<-3
gama<-matrix(c(6,1,-1,1,5,0,-1,0,4),p,p)
Q<-eigen(gama)$vectors
D<-diag(eigen(gama)$values)
A<-Q%*%sqrt(D)%*%t(Q)