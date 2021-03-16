#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))

#==================================================================
#Generate random data to cluster
# ncol =2 dos variables 
# 3 distribuciones 
# clusters muy bien separados (por la media)
# se puede jugar a acercar las medias 5,10,15 o 5,8,11 o 5,7,9 o 5,6,7 (el codo comienza a desaparecer entre más cercanas las medias)
# los clusters se diferencian cada vez menos 
# el programa dice no estoy muy seguro 
# ahora sobrelapamos las 3 con 5,5,5 nosotros sabemos que es idéntica pero el programa aún así clusteriza en 3 pues se lo pedimos 
# entre menos estructura hay el elbow tiende a una estructura recta en donde no hay un cambio de pendiente pues no sabe qué estructura existe 

data <- rbind(matrix(rnorm(300, mean =  5,  sd = 1), ncol = 2),
              matrix(rnorm(300, mean = 12,  sd = 1), ncol = 2),
              matrix(rnorm(300, mean = 19,  sd = 1), ncol = 2))
colnames(data) <- c("x", "y")


#==================================================================
#Run kmeans and generate scatter plot
# metodo no jerarquico, nosotros le decimos el número de clusters

# yo le doy los puntos 
# no le puedes dar dos mismos centroides pues le dices que 5 clusters diferentes pero 
#centroids <- rbind(c(11,11),c(12,12),c(13,13),c(19,19), c(20,20))

# aqui le pedimos 3 clusters y nos da 3, le pedimos 4 y nos dará 4 
# aunque le pido 4 clusters aleatoriamente me divide los 4, no siempre me da los mismos 4 clusters 

# le das los centroides mediante la justificación del conocimiento de campo lo cual es publicable y defendible 
#km<-kmeans(data,centroids)
km <- kmeans(data, 3)
# cluster dentro de km nos da los indices y los usamos para colorear los datos del scatter plot
plot(data, col = km$cluster)
# centers de km son los centroides , se visualiza el centroide de cada grupo
points(km$centers, col = 2:4, pch = 8, cex=3)
# como se comporta la predicción de acuerdo al método de elbow
# kmax es el numero de clusters de acuerdo a los datos que se introdujeron y no corresponde necesariamente al numero biológico
fviz_nbclust(data, FUN = kmeans, method = "wss", k.max = 5, print.summary = TRUE)


#==================================================================
#compare with hclust


hc  <- hclust(dist(data), method = "average")
# coeficiente de correlacion
coeff <- coef(hc)
# cortamos el arbol en 3 
cls3 <- cutree(hc, k=3)
plot(data, col = cls3)

plot (hc, hang=-1)
rect.hclust(hc, k=3, border=2:4)
# varia la kmaxima en el numero de clusters y trata de determinar cual es el número de clusters más adecuado 
fviz_nbclust(data, FUN = hcut, method = "wss", k.max = 5, print.summary = TRUE)





