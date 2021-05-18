library(ggplot2)
library(CCA)
library(factoextra)
library(dplyr)
set.seed(1214747297)
#-----------------------------
# Escalar datos
X <- X_fin_new %>% 
  scale()

Y <- log(Y_com_new+1) %>%
  scale()
#-----------------------------
# CCA
cca_seg <- cancor(X,Y) 
#-----------------------------
# Coeficientes de las variantes X y Y
cca_seg$xcoef
cca_seg$ycoef
#-----------------------------
# Correlacion entre las variantes canonicas 
cca_seg$cor
#-----------------------------
CC1_X <- as.matrix(X) %*% cca_seg$xcoef[, 1]
CC1_Y <- as.matrix(Y) %*% cca_seg$ycoef[, 1]


CC2_X <- as.matrix(X) %*% cca_seg$xcoef[, 2]
CC2_Y <- as.matrix(Y) %*% cca_seg$ycoef[, 2]

cor(CC1_X,CC1_Y)
#-----------------------------
# Para realizar segmentacion por cca
datos_cca <- data.frame(apply(datos, 2, as.numeric))
cca_df <- datos_cca %>% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)

 cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y))+
  geom_point()

kmeans_cca <- kmeans(cca_df[,c(48,49)], 3, iter.max = 1000, nstart = 10)
cca_df$cluster <- kmeans_cca$cluster
ggplot() + geom_point(aes(x=CC1_X,y=CC1_Y, color = cluster), data = cca_df, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans_cca$centers[, 1], y = kmeans_cca$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 3 / K-Medios') + 
  xlab('X') + ylab('Y')


