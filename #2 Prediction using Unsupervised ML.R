
# Cluter

library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(magrittr)
library(clustertend)

library(openxlsx)

# View Data called Iris
View(iris)

# Fetching 4th coloum of Iris Data
Data <-iris[,-c(5)]
Data

# To standarize the variables
Data = scale(Data)
Data

# Assessing cluster tendency
library(clustertend)

set.seed(123)
hopkins(Data, n = nrow(Data)-1)


library(NbClust)
Data_Nb <- NbClust(Data,  distance = "euclidean", min.nc=2, max.nc=15, 
              method = "kmeans",index = "all")

# K-Means Cluster Analysis
Data_Fit <- kmeans(Data,3)
plot(Data,col=Data_Fit$cluster,pch=16) 

# K-Means Cluster Analysis in Btter Way
km.res <- eclust(Data, "kmeans", k = 3, nstart = 25, graph = FALSE)

fviz_cluster(km.res, geom = "point", frame.type = "norm")

# Clustering Validation

# Internal Validation
clmethods <- c("kmeans")
internval <- clValid(Data, nClust = 2:6, clMethods = clmethods, validation = "internal")

# Summary
summary(internval)
optimalScores(internval)

# Stability measure Validation
clmethods <- c("kmeans")
stabval <- clValid(Data, nClust = 2:5, clMethods = clmethods, validation = "stability")

# Display only optimal Scores
summary(stabval)
optimalScores(stabval)


fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())


# Silhouette width of observations
sil <- km.res$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0) 
sil[neg_sil_index, , drop = FALSE]

library(fpc)

# K-Means Cluster Analysis
Data_Fit <- kmeans(Data,3)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(Data), species, Data_Fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand ## value should be maximized
clust_stats$vi  ## value should be minimized
