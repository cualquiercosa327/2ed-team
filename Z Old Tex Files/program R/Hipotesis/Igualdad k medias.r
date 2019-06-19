A<-c(39.4, 41.1, 39.5, 40.0, 43.7, 46.0, 43.5, 42.1)
B<-c(42.7, 39.2, 41.2, 40.7, 37.4, 40.0, 40.7)
C<-c(52.6, 49.4, 49.4, 46.4, 51.2, 49.2, 55.0, 53.6, 55.7, 57.4)
k<-3
alpha<-0.05
n1<-length(A)
n2<-length(B)
n3<-length(C)
mu.comun<-mean(c(A,B,C))
mu1<-mean(A)
mu2<-mean(B)
mu3<-mean(C)
f1<-n1*(mu1-mu.comun)^2+n2*(mu2-mu.comun)^2+n3*(mu3-mu.comun)^2
f2<-var(A)*(n1-1)+var(B)*(n2-1)+var(C)*(n3-1)
estad<-f1/f2
q.val<-pf(estad,k-1,n1+n2+n3-k,lower.tail=F)
estad
qf(1-alpha,k-1,n1+n2+n3-k)
q.val



##

S1<-var(A)
S2<-var(B)
S3<-var(C)
S<-(S1*(n1-1)+S2*(n2-1)+S3*(n3-1))/(n1+n2+n3-k)

cons.c<-1+(1/(n1-1)+1/(n2-1)+1/(n3-1)-1/(n1+n2+n3-k))/(3*(k-1))
estad<-(log(S)*(n1+n2+n3-k)-(n1-1)*log(S1)-(n2-1)*log(S2)-(n3-1)*log(S3))/cons.c
p.val<-pchisq(estad,k-1,lower.tail=F)
estad
qchisq(1-alpha,k-1)
p.val

# usar bartlett.test
ind<-c(rep("A",n1),rep("B",n2),rep("C",n3))
ind
dato<-c(A,B,C)
bartlett.test(dato,ind)