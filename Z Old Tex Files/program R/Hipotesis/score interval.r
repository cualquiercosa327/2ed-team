#    socre.interval

x<-7
n<-30
alpha<-0.05

p.hat<-x/n
x.til<-p.hat+(qnorm(1-alpha/2))^2/(2*n)
n.til<-1+((qnorm(1-alpha/2))^2/n)
p.til<-x.til/n.til
q.til<-1-p.til

LI<-p.til-qnorm(1-alpha/2)*sqrt((p.til*q.til+(qnorm(1-alpha/2))^2/(4*n))/n)/n.til
LS<-p.til+qnorm(1-alpha/2)*sqrt((p.til*q.til+(qnorm(1-alpha/2))^2/(4*n))/n)/n.til

c(LI,LS)
prop.test(7,30,0.5)

