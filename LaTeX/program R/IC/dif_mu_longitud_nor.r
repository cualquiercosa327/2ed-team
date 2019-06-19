nx<-ny<-seq(1,100,1)

f<-function(nx,ny){
fun<-sqrt((1/nx)+(1/ny))
}

longitud<- outer(nx, ny, f)


op <- par(bg = "white")

persp(nx, ny, longitud, theta = 20, phi = 20, expand = 0.8)
