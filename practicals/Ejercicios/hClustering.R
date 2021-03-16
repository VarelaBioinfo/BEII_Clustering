#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))


#read the data
# 3 clusters muy claros en dos variables
#InputData  <- read.table("practicals/Ejercicios/3_clear_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)
# los e clusters están traslapados
#InputData  <- read.table("practicals/Ejercicios/3_ovlp_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)
#las 3 distribuciones tienen la misma media, la misma desviación estandar, son indistinguibles
# a pesar de esto el clustering nos dará resultados aunque sea indistinguible los clusters
InputData  <- read.table("practicals/Ejercicios/no_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)


#==================================================================
#build the dendogram whith hclust
# metodo jerárquico y aglomerativo

hClusters <- hclust(dist(InputData), method = "complete")
# Coeficiente de determinacion, nos dice que tan robusta es la estructura de los clusters, entre más cercano a 1 es mejor 
coeff <- coef(hClusters)
# Graficamos los clusters 
# Sabemos por definición que hay 3 clusters muy claros 
plot (hClusters, hang = -1, main = "hclust Dendogram")
cls3 <- cutree(hClusters, k=3)

#cut the dendogram such that 3 clusters are produced
rect.hclust(hClusters, k=3, border=2:4)
# visualizar el scatter plot 
# 2. la intersección es mayor y es más dificil decidir a cual pertence cual pero sabemos que hay 3 clusters
# 3. sabemos que todos los datos tienen la misma media y stdv entonces solo repartió los datos entre los más externos, son clusters espurios
fviz_cluster(list(data = InputData, cluster = cls3))


#==================================================================
#Build the dendogram with agnes
# Metodo aglomerativo 

# aglomerative nesting, coeficiente de aglomeración 
aClust <- agnes(InputData, method = "complete")
pltree(aClust, cex = 0.6, hang = -1, main = "agnes Dendrogram") 
# nos dice qué tan fuerte es la estructura del cluster en base al metodo de aglomeración usado 
# Entre más fácil es distinguir un cluster del otro más facil es
aCoeff <- aClust$ac
rect.hclust(as.hclust(aClust), k=3, border=2:4)
aCls3 <- cutree(as.hclust(aClust), k = 3)
fviz_cluster(list(data = InputData, cluster = aCls3))


#==================================================================
#Build the dendogram with diana
# jerarquico pero   divisivo

dClust <- diana(InputData)
pltree(dClust, cex = 0.6, hang = -1, main = "diana Dendrogram")
dCoeff <- dClust$dc
rect.hclust(as.hclust(dClust), k=3, border=2:4)
dCls3 <- cutree(as.hclust(dClust), k = 3)
# scatter plot
fviz_cluster(list(data = InputData, cluster = aCls3))


#==================================================================
#Compare two dendograms (slow method)
# Son solo 600 genes pero tarda como 5 minutos generar el entanglement 
#dend1 <- as.dendrogram (hClusters)
#dend2 <- as.dendrogram (as.hclust(aClust))

#dend_list <- dendlist(dend1, dend2)
# Esperamos un entanglement al 100% pues las ramas de los árboles no guardan relación alguna 
#tanglegram(dend1, dend2, main = paste("Entanglement =", round(entanglement(dend_list))))

# Contestar ciertas preguntas 
# media, desviación estandar 
# correr el clustering como una herramienta de exploración en un principio no como toma de decisión
