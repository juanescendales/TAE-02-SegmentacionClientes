library(NbClust)
library(factoextra)

CCA <- read.csv2("CCA.csv")
numero_clusters <- NbClust(data = CCA, distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "alllong")

fviz_nbclust(numero_clusters)