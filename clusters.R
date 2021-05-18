library(NbClust)
library(factoextra)

CCA <- read.csv2("CCA.csv")
numero_clusters <- NbClust(data = CCA, distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "all")



fviz_nbclust(numero_clusters)

kmeans_cca <- kmeans(cca_df[,c(48,49)], iter.max = 1000, nstart = 10)
cca_df$cluster <- kmeans_cca$cluster
ggplot() + geom_point(aes(x=CC1_X,y=CC1_Y, color = cluster), data = cca_df, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans_cca$centers[, 1], y = kmeans_cca$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 4 / K-Medios') + 
  xlab('X') + ylab('Y')
