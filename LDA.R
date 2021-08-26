library(MASS)
library(mosaic)
library(rgl)
library(ggplot2)

#Genero Datos con igual variabilidad en todas las direcciones
set.seed(1)
N = 200
etiq<-c(rep(1,N/2),rep(-1,N/2))
xverde<-rnorm(N/2,0.3,0.1)
yverde<-rnorm(N/2,0.3,0.1)
zverde<-rnorm(N/2,0.3,0.1)
xrojo<-rnorm(N/2,0.7,0.1)
yrojo<-rnorm(N/2,0.7,0.1)
zrojo<-rnorm(N/2,0.7,0.1)
x<-c(xverde,xrojo)
y<-c(yverde,yrojo)
z<-c(zverde,zrojo)
colores<-c(rep("green",N/2),rep("red",N/2))
data= as.data.frame(etiq)
data$x = x
data$y = y
data$z = z

#LDA
prueba.lda<-lda(etiq~x+y+z, data=data) 
prueba.lda
vec_lda  = c(prueba.lda$scaling[1], prueba.lda$scaling[2], prueba.lda$scaling[3])
vec_lda_escalado = vec_lda*-0.15

#Proyecto el punto medio entre las medias de cada grupo
#sobre el vector LDA
xdiv = (mean(xverde)+ mean(xrojo))/2
ydiv = (mean(yverde)+ mean(yrojo))/2
zdiv = (mean(zverde)+ mean(zrojo))/2
punto_div = c(xdiv, ydiv, zdiv)
proj = project(punto_div ~ vec_lda_escalado)
projec_lda  = vec_lda_escalado * proj  

#Gráfico 3D
lim = c(0,1)
plot3d(mean(xverde), mean(yverde), mean(zverde), col = "blue", size = 15, xlim = lim, ylim = lim, zlim = lim) #media verdes
plot3d(mean(xrojo), mean(yrojo), mean(zrojo), col = "blue", size = 15, xlim = lim, ylim = lim, zlim = lim) #media rojos
plot3d(projec_lda[1], projec_lda[2], projec_lda[3], col = "purple", size = 10, xlim = lim, ylim = lim,zlim= lim) #punto de corte
plot3d(x, y, z, col = colores, size = 5, xlim = lim, ylim = lim, zlim = lim) #datos
arrow3d(p0=c(vec_lda_escalado), p1=c(0,0,0),type="lines",barblen = 0.05,col="purple") #vector LDA


#Ahora genero datos con una dispersión distinta en z
rm(list=ls())
set.seed(1)
N = 200 
etiq<-c(rep(1,N/2),rep(-1,N/2))
xverde1<-rnorm(N/2,0.3,0.15)
yverde1<-rnorm(N/2,0.3,0.15)
zverde1<-rnorm(N/2,0.3,0.4)
xrojo1<-rnorm(N/2,0.7,0.15)
yrojo1<-rnorm(N/2,0.7,0.15)
zrojo1<-rnorm(N/2,0.7,0.4)
x1<-c(xverde1,xrojo1)
y1<-c(yverde1,yrojo1)
z1<-c(zverde1,zrojo1)
colores<-c(rep("green",N/2),rep("red",N/2))
data= as.data.frame(etiq)
data$x = x1
data$y = y1
data$z = z1

#LDA
prueba.lda<-lda(etiq ~ x + y + z, data = data) 
prueba.lda
vec_lda  = c(prueba.lda$scaling[1], prueba.lda$scaling[2], prueba.lda$scaling[3])
vec_lda_escalado = vec_lda*-0.25

#Proyecto el punto medio entre las medias de cada grupo
#sobre el vector LDA
xdiv = (mean(xverde1)+ mean(xrojo1))/2
ydiv = (mean(yverde1)+ mean(yrojo1))/2
zdiv = (mean(zverde1)+ mean(zrojo1))/2
punto_div = c(xdiv, ydiv, zdiv)
proj = project(punto_div ~ vec_lda_escalado)
projec_lda  = vec_lda_escalado * proj  

#Dirección que maximiza diferencia entre medias
vec_dif_medias = (c(mean(xrojo1)- mean(xverde1), mean(yrojo1)- mean(yverde1), mean(zrojo1)- mean(zverde1)))

#Gráfico 3D
lim = c(-1, 1.6)
rgl.open()
bg3d(color = "white")
plot3d(mean(xverde1), mean(yverde1), mean(zverde1), col = "blue", size = 10, xlim = lim, ylim = lim,zlim= lim) #media verdes
plot3d(mean(xrojo1), mean(yrojo1), mean(zrojo1), col = "blue", size = 10, xlim = lim, ylim = lim,zlim= lim) #media rojos
plot3d(projec_lda[1], projec_lda[2], projec_lda[3], col = "purple", size = 10, xlim = lim, ylim = lim,zlim= lim) #punto de corte
plot3d(x1, y1, z1, col = colores, size = 5, xlim = lim, ylim = lim, zlim = lim) #datos
arrow3d(p0=c(0,0,0), p1=c( vec_lda_escalado),type="lines", barblen = 0.05, col="purple") #vector LDA
arrow3d(p0=c(0,0,0), p1=c(vec_dif_medias*3),type="lines",barblen = 0.05, col="yellow") #vector diferencia de medias

#Proyecto los datos sobre direccion que solo maximiza diferencia entre medias
vec_dif_medias
proy_m = function(df){
  dir_m = c(0.3914072, 0.3998011, 0.3703228)#=vec_dif_medias
  punto = c(df[2],df[3], df[4] )
  return(project(punto ~ dir_m))
}

proy_dif_medias = apply(data,1, proy_m) 
data$proy_dif_medias = proy_dif_medias
p_corte = project(punto_div ~ vec_dif_medias)

#Gráfico
g <- ggplot(data, aes(x=proy_dif_medias, fill = as.factor(etiq)))
g+ geom_dotplot(alpha=0.4)+
  geom_vline(xintercept = p_corte)+
  scale_fill_manual(values = c("red", "green"))

#Proyecto los datos sobre el vector obtenido por LDA
vec_lda
proy_lda = function(df){
  dir_b = c(-5.0263727, -4.0789172, -0.7815552)#=vec_lda
  punto = c(df[2],df[3], df[4] )
  return(project(punto ~ dir_b))
}

proy_vec_lda = apply(data,1, proy_lda) 
data$proy_vec_lda = proy_vec_lda*-1 
#(invierto la dirección del vecotr para que queden primero los verdes y luego los rojos, como en el gráfico anterior)
punto_corte_lda = project(punto_div ~ vec_lda)

#Gráfico
g <- ggplot(data, aes(x = proy_vec_lda, fill = as.factor(etiq)))
g+ geom_dotplot(alpha=0.4)+
  scale_fill_manual(values = c("red", "green"))+
  geom_vline(xintercept = punto_corte_lda*-1)
