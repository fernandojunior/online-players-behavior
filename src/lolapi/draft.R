# Draft and notes...

data = read.csv("data/ranked_matches_2015_ong_features.csv")
data.categorical = data[,1:3] # apenas atributos categoricos
data.numerical = data[,4:25] # apenas atributos numericos
data.numerical.nbool = data.numerical[,5:22] # apenas atributos numericos sem booleanos
data.numerical.scaled = scale(data.numerical)

# Creates a file with a matrix of attributes correlations
write_matrix_cor <- function() {

    data.scaled = scale(data.numerical)

    correlations = cor(data.scaled)

	correlations.rounded = round(correlations, digits=1)

	write.csv(correlations.rounded, file = "analysis/correlations.csv", sep =",")
}

# A matrix scatter plot for graph correlation analysis 
matrix_scatter_plot <- function () {
	filename = paste("analysis/pairs", ".png", sep="")
	png(file=filename) # abrindo "cursor"  
	pairs(data.numerical)
	dev.off() # "fechando" arquivo	
}

# Generates boxplot graph of the matrix correlations
boxplot_correlations <- function () {

	# Matriz de correlacoes modificado manualmente: 
	# Sem cabecalhos e sem valores nulos (diagonal i==j)
    data = read.csv('analysis/correlations2.csv', header=FALSE, sep=',')

    # valor absoluto dos dados
	data.abs = abs(data)

	# gerando boxplot
	filename = paste("analysis/boxplot_correlations", ".png", sep="")
	png(file=filename) # abrindo "cursor"  
	boxplot(data.abs)
	dev.off() # "fechando" arquivo	

}

fit = kmeans(data.numerical.scaled, 5) # 5 cluster solution

##

d = dist(data.numerical.scaled, method = "euclidean") # distance matrix

fit = hclust(d, method="ward")

plot(fit) # display dendogram

##  k

mydata = data.numerical.scaled

wss = (nrow(mydata)-1) * sum(apply(mydata,2,var))

for  (i  in  2:50) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)

plot(1:50, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




## 

x <- data.numerical.scaled
# run K-Means
km <- kmeans(x, 3, 15)
# print components of km
print(km)
# plot clusters
plot(x, col = km$cluster)
# plot centers
points(km$centers, col = 1:2, pch = 8)

##

library(cluster)
library(fpc) # !!!!!!!!!

dat = data.numerical.scaled[,2:22]

# Kmeans clustre analysis
clus=kmeans(dat, centers=2)

# Fig 01
plotcluster(dat, clus$cluster)

clusplot(dat, clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

## data visualization - how to procedure a pretty plot

library(cluster)

x= data.numerical.scaled[1:3000,]

km=kmeans(x,2)
dissE=daisy(x) 
dE2=dissE^2
sk2=silhouette(km$cl, dE2)
plot(sk2)



## finding k

# See e.g. "Finding the Right Number of Clusters in k­Means and EM Clustering: v­Fold Cross­Validation"
(http://www.statsoft.com/textbook/cluster­analysis/#vfold). Electronic Statistics Textbook. StatSoft. 2010.
Retrieved 2010­05­03

Six methods for determining an optimal k value for k­means analysis
(http://stackoverflow.com/a/15376462/1036500) ­ Answer on stackoverflow containing R code for
several methods of computing an optimal value of k for k­means cluster analysis

## heatmap

x= data.numerical.scaled[1:3000,]

rc = rainbow(nrow(x), start=0, end=.3)
cc = rainbow(ncol(x), start=0, end=.3)

hv = heatmap(x, col = cm.colors(256), scale="column", RowSideColors = rc, ColSideColors = cc, margin=c(5,10), xlab = "specification variables", ylab= "Car Models", main = "Heatmap of mtcars data")


## algorithms

hewson_notes.pdf

There are a wide range of algorithms that have been developed to investigate clustering within data.
These can be considered in a number of ways:
• Hierarchical Methods
– Agglomerative clustering (hclust(), agnes())
– Divisive clustering (diana(), mona())
• Partitioning methods (kmeans(), pam(), clara())

## 

USArrests = data.numerical.scaled

US.km <- kmeans(USArrests, centers = 12)
plot(USArrests, col = US.km$cluster, pch = US.km$cluster) ## not shown
plot(prcomp(USArrests, center = TRUE)$x[,c(1,2)], col = US.km$cluster, pch = US.km$cluster)

library("scatterplot3d")

scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,2,3)], pch = US.km$cluster, type = "h", angle = 55, color = US.km$cluster)

scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,5,7)], pch = US.km$cluster, type = "h", 
angle = 95, color = US.km$cluster)

scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,2,3)], pch = US.km$cluster, type = "h", angle = 95, color = US.km$cluster)


scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,6,9)], pch = US.km$cluster, type = "h", angle = 95, color = US.km$cluster)


scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(8,9,12)], pch = US.km$cluster, type = "h", angle = 95, color = US.km$cluster)

scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,2,3)], pch = US.km$cluster, type = "h", angle = 190, color = US.km$cluster)

scatterplot3d(prcomp(USArrests, center = TRUE)$x[,c(1,2,3)], pch = US.km$cluster, type = "h", angle = 90+90+90+90+90+2, color = US.km$cluster)

