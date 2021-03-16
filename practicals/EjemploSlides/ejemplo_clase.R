#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))

#read the data
InputData  <- read.table("practicals/EjemploSlides/ejemplo_clase.csv", header = TRUE, sep = ",", quote="", row.names = 1)

# haacer la matriz de distancia
#Run the hierarchical clustering and plot the dendogram
ccom <- hclust(dist(InputData), method = "complete")
plot (ccom, hang = -1)

#Save tree in Newick format
my_tree <- as.phylo(ccom)
write.tree(phy=my_tree, file="ejemplo_clase.tree")  

#cut the dendogram such that 3 clusters can be visualized
rect.hclust(ccom, k=3, border=2:4)

#get the data points within each cluster
# Cortar para obtener los inices de cada observación
cls3 <- cutree(ccom, k=3)

#Scatter plot to visualize the data points in each cluster
plot(InputData, xlim=c(0,8), ylim=c(0,8), col=cls3)
# Graficamos dos variables, libreria que te pone los centroides y más cosas
# lo mismo que arriba pero con más estética
fviz_cluster(list(data = InputData, cluster = cls3))

#==================================================================
#Now compare with method single linkage
# la distancia son los dos puntos más cercanos a ese cluster 
csin <- hclust(dist(InputData), method = "single")
plot (csin, hang = -1)
# se corta el arbol en donde tenemos los 3 clusters
rect.hclust(csin, k=3, border=2:4)

# Comparar dos dendogramas para ver que tan congruentes fueron los arboles 
dend1 <- as.dendrogram (ccom)
dend2 <- as.dendrogram (csin)

# Generamo suna lista con los dos dendogramas y los comparamos
dend_list <- dendlist(dend1, dend2)
# Pone los dos dendogramas el uno contra el otro y muestra en donde están las diferencias 
# Entanglemente puede variar entre 0 (no hay enmarañamiento entonces los árboles son idénticos) 1 (los árboles son completamente distintos)
# Toma la proporción de cuantas hojas fueron clasificadas de la misma manera (método recursivo)
tanglegram(dend1, dend2, main = paste("Entanglement =", round(entanglement(dend_list))))


#==================================================================
# Determine the number of clusters

#Methods: Total Within Sum of Squares (wss), silhouette, gap_stat
# Calcular automáticamente el número de clusters wss metodo de elbow (se forma el codo)
# widden sum square
fviz_nbclust(InputData, FUN = hcut, method = "wss", k.max = 6, print.summary = TRUE)
# metodo de silueta en el que determina la magnitud de la distancia que hay entre una observación con su clustering 
# Cuando hay 3 clusters que tan solida se ve la pertenencia de cada una con su cluster. 
fviz_nbclust(InputData, FUN = hcut, method = "silhouette", k.max = 6, print.summary = TRUE)
# Gap stat  
fviz_nbclust(InputData, FUN = hcut, method = "gap_stat", k.max = 6, print.summary = TRUE)
