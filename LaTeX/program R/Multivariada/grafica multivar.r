library(mnormt)
set.seed(123456)

# simular peso y estatura
b<-rmnorm(n=200,c(171,67),varcov=matrix(c(400,150,150,100),2,2))
a<-data.frame(t(b),row.names=c("Estatura","peso"))

panel.hist <- function(x, ...)
{
usr <- par("usr"); on.exit(par(usr))
#para definir regi\'on de graficiaci\'on
par(usr = c(usr[1:2], 0, 1.5) )
#para obtener una lista que guarde las
#marcas de clase y conteos en cada una:
h <- hist(x, plot = FALSE)
breaks <- h$breaks;
nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col=8, ...)
#para dibujar los histogramas
}


pairs(t(a), panel=panel.smooth, cex = 1.5,
pch = 19, bg="light blue",
diag.panel=panel.hist, cex.labels = 1, font.labels=1)
par(oma=c(1,1,1,1),new=T,font=2,cex=0.5)
mtext(outer=T,"",side=3)

