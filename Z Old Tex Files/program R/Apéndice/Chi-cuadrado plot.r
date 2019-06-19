chi.plot<-function(x){
x<-data.frame(x)
n<-dim(x)[1]
p<-dim(x)[2]
d2<-rep(NA,n)
bar<-mean(x)
S<-var(x)
for(i in 1:n){
d2[i]<-mahalanobis(x[i,],bar,S)
}
j<-c(1:n)
percen<-qchisq((j-0.5)/n,p)
plot(percen,sort(d2),main="plot chi cuadrado")
abline(0,1)
}

ante<-c(230,245,220,250, 260,250,220,300,310,290,260,240,210, 220,250,245,274,230,285,275)
desp<-c(210,230,215,220,240,220,210,260,280,270,230,235,200, 200,210,230,250,210,260,230)
X<-data.frame(cbind(ante,desp))
chi.plot(X)

which(d2==max(d2))