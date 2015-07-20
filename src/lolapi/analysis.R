
library("car")

data = read.csv("data/ranked_matches_2015_ong_features.csv")
data.categorical = data[,1:3] # apenas atributos categoricos
data.numerical = data[,4:25] # apenas atributos numericos
data.numerical.nbool = data.numerical[,5:22] # apenas atributos numericos sem booleanos
data.numerical.scaled = scale(data.numerical)

# A matrix scatter plot for graph correlation analysis 
matrix_scatter_plot <- function () {
	filename = paste("analysis/pairs", ".png", sep="")
	png(file=filename) # abrindo "cursor"  
	pairs(data.numerical)
	dev.off() # "fechando" arquivo	
}

# boxplot of attribute correlations
boxplot_correlations <- function () {

	# matriz de correlacoes sem cabecalhos e sem a diagonal i==j
    data = read.csv('analysis/correlations2.csv', header=FALSE, sep=',')

	mod = sqrt(data*data)

	filename = paste("analysis/boxplot_correlations", ".png", sep="")
	png(file=filename) # abrindo "cursor"  
	boxplot(mod)
	dev.off() # "fechando" arquivo	

}

# write attr. correlations into a file
write_matrix_cor <- function() {

    data.scaled = scale(data.numerical)

    correlations = cor(data.scaled)

	correlations.rounded = round(correlations, digits=1)

	write.csv(correlations.rounded, file = "analysis/correlations.csv", sep =",")
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

## implementing kmeans


# removendo outliers

options(scipen=999)

data = read.csv("data/ranked_matches_2015_ong_features.csv")

#selecting only numerical data and removing ac_id column
# First check the complete set of components for outliers
boxplot(data[, 8:25]) # apenas numericos
boxplot(data[, 4:25]) # apenas numericos

# As we can see from the above plots that avg_sal,unemp_rate and loan amount has some outliers in the data. Let’s analyze all these three individually.

analysis.outliers = function (title, data, scatterplot=TRUE, names=FALSE) {
	title = paste(title, "Boxplot")
	filename = paste("analysis/outliers/", title, ".png", sep="")
	png(file=filename) # abrindo "cursor"

	if (names == TRUE)
		boxplot(data, main=title, names=c(1:ncol(data)))
	else
		boxplot(data, main=title)
	dev.off() # "fechando" arquivo

	if(scatterplot == TRUE) {
		title = paste(title, "Scatterplot")
		filename = paste("analysis/outliers/", title, ".png", sep="")
		png(file=filename) # abrindo "cursor"
		plot(data, main=title)
		dev.off() # "fechando" arquivo
	}
	
}

analysis.outliers('Todos os atributos', data[, 4:25], FALSE, TRUE) # apenas numericos

analysis.outliers('[4] Kills', data$Kills)
analysis.outliers('[5] Assists', data$Assists)
analysis.outliers('[6] Deaths', data$Deaths)
analysis.outliers('[7] GoldEarned', data$GoldEarned)
analysis.outliers('[8] TotalDamageDealt', data$TotalDamageDealt)
analysis.outliers('[9] MagicDamageDealt', data$MagicDamageDealt)
analysis.outliers('[10] PhysicalDamageDealt', data$PhysicalDamageDealt)
analysis.outliers('[11] TotalDamageDealtToChampions', data$TotalDamageDealtToChampions)
analysis.outliers('[12] TotalDamageTaken', data$TotalDamageTaken)
analysis.outliers('[13] MinionsKilled', data$MinionsKilled)
analysis.outliers('[14] NeutralMinionsKilled', data$NeutralMinionsKilled)
analysis.outliers('[15] CrowdControl', data$CrowdControl)
analysis.outliers('[16] WardsPlaced', data$WardsPlaced)
analysis.outliers('[17] TowerKills', data$TowerKills)
analysis.outliers('[18] LargestMultiKill', data$LargestMultiKill)
analysis.outliers('[19] LargestKillingSpree', data$LargestKillingSpree)
analysis.outliers('[20] LargestCritStrike', data$LargestCritStrike)
analysis.outliers('[21] TotalHealAmount', data$TotalHealAmount)

data = data[data$Kill < 35, ]
data = data[data$Assists < 40, ]
data = data[data$Deaths < 30, ]
data = data[data$GoldEarned < 26250, ]
data = data[data$TotalDamageDealt < 500000, ]
data = data[data$MagicDamageDealt < 350000, ]
data = data[data$PhysicalDamageDealt < 450000, ]
data = data[data$TotalDamageDealtToChampions < 80000, ]
data = data[data$TotalDamageTaken < 80000, ]
data = data[data$MinionsKilled < 400, ]
data = data[data$NeutralMinionsKilled < 125, ]
data = data[data$CrowdControl < 10000, ]
data = data[data$WardsPlaced < 110, ]
data = data[data$TowerKills < 9, ]
data = data[data$LargestMultiKill < 6, ]
data = data[data$LargestKillingSpree < 21, ]
data = data[data$LargestCritStrike < 2500, ]
data = data[data$TotalHealAmount < 40000, ]

analysis.outliers('Todos os atributos (pos)', data[, 4:25], FALSE, TRUE) # apenas numericos


#Since the data attributes are of different varieties their scales are also different. In order to maintain uniform scalability we scale the columns.

data.categorical = data[,1:3] # apenas atributos categoricos
data.booleans = data[,4:7] # atributos booleanos
data.numerical = data[, 8:25] # atributos numericos
data.numerical.scaled = scale(data.numerical)


# Calculating variance and storing at the first index in wss

wss <- (nrow(data.numerical)-1)*sum(apply(data.numerical,2,var))

for(i in 2:50)wss[i]<- sum(fit=kmeans(data.numerical,centers=i,50)$withinss)
plot(1:50,wss,type="b",main="50 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")

fit <- kmeans(data.numerical.scaled[1:1000,],2)

library(cluster)

clusplot(data.numerical[1:80,], fit$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(data.numerical.scaled[1:200,],4)
clusplot(data.numerical[1:80,], fit$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(data.numerical.scaled, 4, algorithm='Lloyd')


# removendo matchids com menos de 10 participantes

contains = function (x, l) {
	for (i in l)
		if (x == i)
			return(TRUE)
	return(FALSE)
}

# retorna matches que nao tem todos os participantes (<10) ...
matchs_without_participants = function () {

	# matches to be removed
	to_be_removed = c()

	# matches already verified
	already_verified = c()

	for(i in c(1:nrow(data))){

	    participant = data[i,]
	    matchId = participant$matchId

	    if(!contains(matchId, already_verified)) {

	    	already_verified = c(already_verified, matchId)

		    total_participants = nrow(data[data$matchId == matchId,])

		    if (total_participants != 10)
		    	to_be_removed = c(to_be_removed, matchId)

	    }

	}

	return(to_be_removed)
	
}

to_be_removed = matchs_without_participants()

data = data[!(data$matchId %in% to_be_removed), ]

data.normalized = cbind(data[,1:3], data[,4:7], scale(data[,8:25]))

correlations = cor(data.normalized[,4:25])

correlations.rounded = round(correlations, digits=1)

correlations.mod = sqrt(correlations.rounded * correlations.rounded)

write.csv(correlations.mod, file = "analysis/correlations_filtered_mod.csv", sep =",")

correlations.boxplot = read.csv('analysis/correlations_filtered_mod_boxplot.csv', header=FALSE)

analysis.outliers('Correlações de atributos', correlations.boxplot, FALSE, TRUE) # apenas numericos

# reducao atributos

data.reduzido = data.normalized[,4:25]
data.reduzido = cbind(data.reduzido[,c(1,5)], data.reduzido[,7:14], data.reduzido[,17:21])

ldata = data.reduzido

wss <- (nrow(ldata)-1)*sum(apply(ldata,2,var))

max = 50

for(i in 2:max)wss[i]<- sum(fit=kmeans(ldata,centers=i,max)$withinss)

plot(1:max,wss,type="b",main="max clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")

fit <- kmeans(ldata, 11, algorithm='Lloyd')

library(cluster)

fit <- kmeans(ldata, 4, algorithm='Lloyd')
clusplot(ldata[1:80,], fit$cluster[1:80], color=FALSE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(ldata[1:200,],4)
clusplot(ldata[1:80,], fit$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(data.numerical.scaled, 4, algorithm='Lloyd')

