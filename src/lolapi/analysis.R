library("car")
library(cluster)  # for clusplot
library("scatterplot3d")  # scatterplot3d

options(scipen=999)

# Some functions
# --------------

# Return TRUE if an object is in list, FALSE otherwise
contains = function (obj, list_) {
	for (i in list_)
		if (obj == i)
			return(TRUE)
	return(FALSE)
}

# Return total sum of squares of multidimensional X sample data: (n-1) * Var(X)
sum_of_squares = function (X) {
	n = nrow(X) # size
	VarX = sum(apply(X,2,var))  # variance
	TSS = ((n-1) * VarX) # total sum of square
	return(c(n, TSS, VarX)) # return the values
}

# Return TRUE if s starts with the specified prefix, FALSE otherwise.
startswith = function (s, prefix) {
	return(grepl(paste('^', prefix), s))
}

# Return TRUE if s ends with the specified suffix, FALSE otherwise.
endswith = function (s, suffix) {
	return(grepl(paste(suffix, '$', sep=''), s))
}

# Create a boxplot
plot.boxplot = function (...) {
	args = list(...)
	do.call(boxplot, args)
}

# Create a scatterplot
plot.scatter = function (...) {
	args = list(...)
	do.call(plot, args)
}

# Save the output of a function in a png file.
save_png = function (filename, fn, ...) {
	if (!endswith(filename, '.png'))
		filename = paste(filename, '.png', sep='')
	png(file=filename)
	fn(...)
	dev.off()
}

# Create a boxplot and save the output in a png file.
save_boxplot = function (title, data, ...) {
	filename = paste(BOXPLOTS, title, sep='')
	save_png(filename, plot.boxplot, data, main=title , ...)
}

# Create a scatterplot and save the output in a png file.
save_scatterplot = function (title, data, ...) {
	filename = paste(SCATTERPLOTS, title, sep='')
	save_png(filename, plot.scatter, data, main=title, ...)
}

# Create box and scatter plots and save the outputs in png files.
save_plots = function (title, data) {
	save_boxplot(title, data)
	save_scatterplot(title, data)
}

# Constants
# ---------

BOXPLOTS = 'data/boxplots/'

SCATTERPLOTS = 'data/scatterplots/'

# Load data
# -----------

data = read.csv("data/ranked_matches_2015_ong_features.csv")

# attributes range:
# info = 1:3
# boolean = 4:7
# numerical = 8:25

# Outliers analysis
# -----------------

# analyzing all attributes using boxplot
save_boxplot('All attributes', data[, 4:25], names=c(1:ncol(data[, 4:25])))

# As we can see from the above plot that some attributes has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
save_plots('[4] Kills', data$Kills)
save_plots('[5] Assists', data$Assists)
save_plots('[6] Deaths', data$Deaths)
save_plots('[7] GoldEarned', data$GoldEarned)
save_plots('[8] TotalDamageDealt', data$TotalDamageDealt)
save_plots('[9] MagicDamageDealt', data$MagicDamageDealt)
save_plots('[10] PhysicalDamageDealt', data$PhysicalDamageDealt)
save_plots('[11] TotalDamageDealtToChampions', data$TotalDamageDealtToChampions)
save_plots('[12] TotalDamageTaken', data$TotalDamageTaken)
save_plots('[13] MinionsKilled', data$MinionsKilled)
save_plots('[14] NeutralMinionsKilled', data$NeutralMinionsKilled)
save_plots('[15] CrowdControl', data$CrowdControl)
save_plots('[16] WardsPlaced', data$WardsPlaced)
save_plots('[17] TowerKills', data$TowerKills)
save_plots('[18] LargestMultiKill', data$LargestMultiKill)
save_plots('[19] LargestKillingSpree', data$LargestKillingSpree)
save_plots('[20] LargestCritStrike', data$LargestCritStrike)
save_plots('[21] TotalHealAmount', data$TotalHealAmount)

# removing participants with large outliers
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

# analyzing attributes after removal of large outliers
save_boxplot('All attributes (pos)', data[, 4:25], names=c(1:ncol(data)))

# Apos a remocao dos participantes com large outilies
# alguns matches ficaram com menos de 10 participantes ...
# Essa funcao retorna matches que nao tem todos os participantes (<10) ...
# TODO utilizar aggregate
# aggregate(summonerId ~ matchId, data=data, FUN = function(x) length(x))
matchs_without_participants = function () {

	# matches without 10 participants
	without_participants = c()

	# match IDs already verified
	already_verified = c()

	for(i in c(1:nrow(data))){

	    participant = data[i,]

	    matchId = participant$matchId

	    if(!contains(matchId, already_verified)) {

	    	# total of participants in match
		    total_participants = nrow(data[data$matchId == matchId,])

		    if (total_participants != 10)
		    	without_participants = c(without_participants, matchId)

	    	already_verified = c(already_verified, matchId)

	    }

	}

	return(to_be_removed)

}

# removendo matches que nao tem todos os 10 participantes
data = data[!(data$matchId %in% matchs_without_participants()), ]

# persisting data with outliers softened
write.csv(data, file = "data/ranked_matches_2015_no_largeoutliers.csv")

# Data split
# ----------

# info attributes
data.info = data[, 1:3]

# bool attributes
data.bool = data[, 4:7]

# numerical attributes
data.numerical = data[, 8:25]

# Data normalization (z-score)
# ------------------

# Since the data attributes are of different varieties their scales are also
# different. In order to maintain uniform scalability we scale (z-score) the
# numerical attributes.
data.normalized = cbind(data.info, data.bool, scale(data.numerical))

# Correlation analysis
# --------------------

# correlation between booelean and numerical attributes
correlations = cor(data.normalized[,4:25])
correlations.rounded = round(correlations, digits=1)
correlations.abs = abs(correlations.rounded)
write.csv(correlations.abs, file = "analysis/correlations_filtered_mod.csv", sep =",")

# dados tratados (sem diagonal e header) das correlacoes para o boxplot
correlations.boxplot = read.csv('analysis/correlations_filtered_mod_boxplot.csv', header=FALSE)

# boxplot das correlacoes
save_boxplot('Attributes correlation', correlations.boxplot)

# Attribute selection
# -------------------

# boolean and numerical attributes
data.reduzido = data.normalized[, 4:25]

# selecting attributes based on correlation analysis
data.reduzido = data.reduzido[, c(5, 7:14, 17: 21)]

# renomeando para melhor manipulacao
ldata = data.reduzido

# K-means analysis
# ----------------

# calculating total sum of squares and storing at the first index in tss
tss <- (nrow(ldata)-1)*sum(apply(ldata,2,var))

# max value of k
max = 50

# calculating the total sum of squares for each k == i and storing in tss[i]
for(i in 2 : max)
    tss[i] = sum(fit = kmeans(ldata, centers = i, max, algorithm = 'Lloyd')$withinss)

# analysing the knees of the plot to find the ideal k number of cluster
plot(
    1:max,
    tss,
    type = "b",
    main = "K clusters",
    xlab = "No. of cluster",
    ylab = "Cluster sum of squares")

# best k == 8

# Some tests
# ----------

fit <- kmeans(ldata, 4, algorithm='Lloyd')

fit <- kmeans(ldata[1:200,],4)

scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(1,2,3)], pch = fit$cluster, type = "h", angle = 55, color = fit$cluster)

scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(1,2,3)], pch = fit$cluster, type = "h", angle = 95, color = fit$cluster)

# most correlated {3: goldEarned, 4: totalDamageDealt, 9: minionsKilled}
scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(3,4,9)], pch = fit$cluster, type = "h", angle = 95, color = fit$cluster)

# merge com dados nominais sobre os participantes, stats, e cluster
ldata2 = cbind(data[, c(1,2,3,4)], ldata, cluster)

# dispersao com cluster dos participantes perdedores
perdedores = ldata2[ldata2$Win == 0, 5:(ncol(ldata2))]
plot(perdedores[,c(3,4,9)], col=perdedores$cluster, pch=15)

# dispersao com cluster dos participantes perdedores
vencedores = ldata2[ldata2$Win == 1, 5:(ncol(ldata2))]
plot(vencedores[, c(3,4,9)], col=vencedores$cluster, pch=15)

# scatterplot most cor dos perdedores
scatterplot3d(prcomp(perdedores, center = TRUE)$x[,c(3,4,9)], pch = perdedores$cluster, type = "h", angle = 95, color = perdedores$cluster)

# corrigido: apenas inteiros ... attr cluster nao considerado
scatterplot3d(prcomp(perdedores[,1:(ncol(perdedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = perdedores$cluster, type = "h", angle = 95, color = perdedores$cluster)

scatterplot3d(prcomp(vencedores, center = TRUE)$x[,c(3,4,9)], pch = vencedores$cluster, type = "h", angle = 95, color = vencedores$cluster)

# corrigido: apenas inteiros ... attr cluster nao considerado
scatterplot3d(prcomp(vencedores[,1:(ncol(vencedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = vencedores$cluster, type = "h", angle = 95, color = vencedores$cluster)

# Testing k-means configurations
# ------------------------------

fit = kmeans(ldata, 11, algorithm='Lloyd')
# (between_SS / total_SS =  61.2 %)
# Warning: did *not* converge in specified number of iterations

fit2 = kmeans(ldata, 11, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  61.9 %)

fit3 = kmeans(ldata, 8, algorithm='Lloyd')
# (between_SS / total_SS =  57.2 %)
# Warning: did *not* converge in specified number of iterations

fit4 = kmeans(ldata, 8, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  57.7 %)

# persisting k-means results
write.csv(fit$cluster, file = "analysis/cluster/testes/fit$cluster.csv")
write.csv(fit2$cluster, file = "analysis/cluster/testes/fit2$cluster.csv")
write.csv(fit3$cluster, file = "analysis/cluster/testes/fit3$cluster.csv")
write.csv(fit4$cluster, file = "analysis/cluster/testes/fit4$cluster.csv")

write.csv(fit$centers, file = "analysis/cluster/testes/fit$centers.csv")
write.csv(fit2$centers, file = "analysis/cluster/testes/fit2$centers.csv")
write.csv(fit3$centers, file = "analysis/cluster/testes/fit3$centers.csv")
write.csv(fit4$centers, file = "analysis/cluster/testes/fit4$centers.csv")

write.csv(fit$size, file = "analysis/cluster/testes/fit$size.csv")
write.csv(fit2$size, file = "analysis/cluster/testes/fit2$size.csv")
write.csv(fit3$size, file = "analysis/cluster/testes/fit3$size.csv")
write.csv(fit4$size, file = "analysis/cluster/testes/fit4$size.csv")

write.csv(fit$totss, file = "analysis/cluster/testes/fit$totss.csv")
write.csv(fit2$totss, file = "analysis/cluster/testes/fit2$totss.csv")
write.csv(fit3$totss, file = "analysis/cluster/testes/fit3$totss.csv")
write.csv(fit4$totss, file = "analysis/cluster/testes/fit4$totss.csv")

write.csv(fit$tot.withinss, file = "analysis/cluster/testes/fit$tot.withinss.csv")
write.csv(fit2$tot.withinss, file = "analysis/cluster/testes/fit2$tot.withinss.csv")
write.csv(fit3$tot.withinss, file = "analysis/cluster/testes/fit3$tot.withinss.csv")
write.csv(fit4$tot.withinss, file = "analysis/cluster/testes/fit4$tot.withinss.csv")

write.csv(fit$withinss, file = "analysis/cluster/testes/fit$withinss.csv")
write.csv(fit2$withinss, file = "analysis/cluster/testes/fit2$withinss.csv")
write.csv(fit3$withinss, file = "analysis/cluster/testes/fit3$withinss.csv")
write.csv(fit4$withinss, file = "analysis/cluster/testes/fit4$withinss.csv")

write.csv(fit$betweenss, file = "analysis/cluster/testes/fit$betweenss.csv")
write.csv(fit2$betweenss, file = "analysis/cluster/testes/fit2$betweenss.csv")
write.csv(fit3$betweenss, file = "analysis/cluster/testes/fit3$betweenss.csv")
write.csv(fit4$betweenss, file = "analysis/cluster/testes/fit4$betweenss.csv")

write.csv(fit$iter, file = "analysis/cluster/testes/fit$iter.csv")
write.csv(fit2$iter, file = "analysis/cluster/testes/fit2$iter.csv")
write.csv(fit3$iter, file = "analysis/cluster/testes/fit3$iter.csv")
write.csv(fit4$iter, file = "analysis/cluster/testes/fit4$iter.csv")

write.csv(fit$ifault, file = "analysis/cluster/testes/fit$ifault.csv")
write.csv(fit2$ifault, file = "analysis/cluster/testes/fit2$ifault.csv")
write.csv(fit3$ifault, file = "analysis/cluster/testes/fit3$ifault.csv")
write.csv(fit4$ifault, file = "analysis/cluster/testes/fit4$ifault.csv")

# variance of each cluster
fit4$withinvar = 1 / (fit4$size - 1) * fit4$withinss
# TODO put in csv file

# Scatterplot of clusterized data (fit4)
# --------------------------------------

# clusplot of sampled n=80 data
clusplot(ldata[1:80,], fit4$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)
legend("bottomleft", legend = paste("Group", 1:8), pch=1, col=1:8)

# scatterplot of all attributes # TODO put in csv file
plot(ldata, col=fit4$cluster, pch=15)

# only most correlated attributes # TODO put in csv file
plot(ldata[, c(3,4,9)], col=fit4$cluster, pch=15)

# associating each participant tuple with its cluster
ldata2 = cbind(ldata, Cluster=fit4$cluster)

# spliting clusterized data between winners and losers
vencedores = ldata2[ldata2$Win == 1,]
perdedores = ldata2[ldata2$Win == 0,]

# scatterplot of each partition # TODO put in csv file
plot(vencedores[, c(3,4,9)], col=vencedores$Cluster, pch=15)
plot(perdedores[, c(3,4,9)], col=perdedores$Cluster, pch=15)

# 3D scatterplot using PCA of clusterized data
----------------------------------------------

# 3D scatterplot of most correlated attributes
scatterplot3d(
	prcomp(ldata, center = TRUE)$x[, c(3,4,9)],
	pch=fit4$cluster,
	type="h",
	angle=95,
	color=fit4$cluster)

# 3D scatterplot of most correlated attributes of winners # TODO put in csv file
scatterplot3d(
	prcomp(vencedores[,1:(ncol(vencedores)-1)],
	center=TRUE)$x[,c(3,4,9)],
	pch=vencedores$Cluster,
	type="h",
	angle=95,
	color=vencedores$Cluster)

# 3D scatterplot of most correlated attributes of losers # TODO put in csv file
scatterplot3d(
	prcomp(perdedores[, 1:(ncol(perdedores)-1)],
	center=TRUE)$x[,c(3,4,9)],
	pch=perdedores$Cluster,
	type="h",
	angle=95,
	color=perdedores$Cluster)

# Summarying partitions
-----------------------

# within cluster sum of squares
withinss = function (data, cluster) {
	X = data[data$Cluster == cluster, ]
	return(sum_of_squares(X))
}

vencedores = c('size', 'withinss', 'var')
perdedores = c('size', 'withinss', 'var')

for(i in c(1:8)) {
	vencedores = rbind(i=vencedores, withinss(vencedores, i))
	perdedores = rbind(i=perdedores, withinss(perdedores, i))
}

# winners output TODO put in csv file
# 3739 41316.9371603666 11.0532202141163
# 4281 19268.3432556076 4.50194935878682
# 5223 49015.2318475736 9.38629487697695
# 5179 15236.4693924323 2.94253947323915
# 4035 27864.5755564661 6.90743072792912
# 3691 18353.4704144465 4.97384022071721
# 5835 46343.2471402998 7.94364880704487
# 9667 52208.3269817618 5.40123391079679

# losers output TODO put in csv file
# 1674 16687.9878413122 9.97488812989375
# 3772 16583.1388273172 4.39754410695233
# 3217 25773.5785903237 8.01417244723994
# 11838 34719.6991407263 2.9331502188668
# 6453 39535.8716607882 6.12769244587541
# 10711 51691.7332768328 4.82649236945218
# 418 2547.04791512543 6.10802857344227
# 3567 14730.9541129319 4.13094618982947

# TODO centers of each partition ... scaled and not scaled

# mean of all attributes (not scaled) grouped by clusters
tmp = cbind(data.bool, data.numerical)
tmp = tmp[, c(5, 7:14, 17: 21)]
centers_not_scaled = aggregate(. ~ Cluster, tmp, mean)
write.csv(centers_not_scaled[, c(2:ncol(centers_not_scaled))], file = "analysis/cluster/testes/fit4/centers_not_scaled.csv")

# TODO
# cluster, participants, winners, losers, winrate (w/(w+l))
# 1 5413 3739 1674 0.6907445039719194
# 2 8053 4281 3772 0.5316031292685955
# 3 8440 5223 3217 0.6188388625592417
# 4 17017 5179 11838 0.304342716107422
# 5 10488 4035 6453 0.3847254004576659
# 6 14402 3691 10711 0.25628384946535204
# 7 6253 5835 418 0.9331520869982408
# 8 13234 9667 3567 0.7304669789935015

### H1-0: Não existe diferença entre as distribuições dos clusters encontrados no modelo de aprendizagem

# normalized relative weight and normalized principal Eigen vector [Kardi Teknomo]

data.normalized_relative_weight = data[,4:25]
data.normalized_relative_weight = cbind(data.normalized_relative_weight[,c(1,5)], data.normalized_relative_weight[,7:14], data.normalized_relative_weight[,17:21])
data.normalized_relative_weight = data.normalized_relative_weight[,2:ncol(data.normalized_relative_weight)]

data.normalized_relative_weight = data.matrix(data.normalized_relative_weight)
data.normalized_relative_weight = data.normalized_relative_weight / sum(data.normalized_relative_weight)

ttt = data.normalized_relative_weight + 0.0
cbind(
   ttt[,1]/sum(ttt[,1]),
   ttt[,2]/sum(ttt[,2]),
   ttt[,3]/sum(ttt[,3]),
   ttt[,4]/sum(ttt[,4]),
   ttt[,5]/sum(ttt[,5]),
   ttt[,6]/sum(ttt[,6]),
   ttt[,7]/sum(ttt[,7]),
   ttt[,8]/sum(ttt[,8]),
   ttt[,9]/sum(ttt[,9]),
   ttt[,10]/sum(ttt[,10]),
   ttt[,11]/sum(ttt[,11]),
   ttt[,12]/sum(ttt[,12]),
   ttt[,13]/sum(ttt[,13]),
   ttt[,14]/sum(ttt[,14])
)

data.normalized_relative_weight = cbind(
   ttt[,1]/sum(ttt[,1]),
   ttt[,2]/sum(ttt[,2]),
   ttt[,3]/sum(ttt[,3]),
   ttt[,4]/sum(ttt[,4]),
   ttt[,5]/sum(ttt[,5]),
   ttt[,6]/sum(ttt[,6]),
   ttt[,7]/sum(ttt[,7]),
   ttt[,8]/sum(ttt[,8]),
   ttt[,9]/sum(ttt[,9]),
   ttt[,10]/sum(ttt[,10]),
   ttt[,11]/sum(ttt[,11]),
   ttt[,12]/sum(ttt[,12]),
   ttt[,13]/sum(ttt[,13]),
   ttt[,14]/sum(ttt[,14])
)

# dados normalizados pelo peso relativo
kruskal.test(rowSums(data.normalized_relative_weight), fit4$cluster)

	Kruskal-Wallis rank sum test

data:  rowSums(data.normalized_relative_weight) and fit4$cluster
Kruskal-Wallis chi-squared = 70612, df = 7, p-value < 2.2e-16

# dados normalizado z-score
kruskal.test(rowSums(ldata), fit4$cluster)

	Kruskal-Wallis rank sum test

data:  rowSums(ldata) and fit4$cluster
Kruskal-Wallis chi-squared = 70036, df = 7, p-value < 2.2e-16

# analisando pela soma dos quadrados
wilcox.test(fit4$withinss, conf.int=T)

	Wilcoxon signed rank test

data:  fit4$withinss
V = 36, p-value = 0.007813
alternative hypothesis: true location is not equal to 0
95 percent confidence interval:
 49032.26 73588.02
sample estimates:
(pseudo)median
      62429.24

### H1-1: Para cada cluster encontrado, existe diferença entre as medianas dos jogadores vitoriosos e perdedores

x = rowSums(vencedores[ vencedores$Cluster == 1,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 1,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 3275300, p-value = 0.006096
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 2,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 2,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 8345600, p-value = 0.00907
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 3,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 3,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 9178000, p-value = 0.0000000000008951
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 4,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 4,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 30907000, p-value = 0.3918
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 5,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 5,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 14623000, p-value < 0.00000000000000022
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 6,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 6,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 23422000, p-value < 0.00000000000000022
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 7,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 7,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 1088700, p-value = 0.0002433
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 8,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 8,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
	Wilcoxon rank sum test with continuity correction
data:  x and y
W = 17823000, p-value = 0.002865
alternative hypothesis: true location shift is not equal to 0


# References
# [Kardi Teknomo] Kardi Teknomo. ANALYTIC HIERARCHY PROCESS (AHP) TUTORIAL. https://docs.google.com/file/d/0BxYU82vErc8xYS1PendBeHlpSkk/edit?usp=sharing
# http://stats.stackexchange.com/questions/49521/how-to-find-variance-between-multidimensional-points
# http://www.edureka.co/blog/clustering-on-bank-data-using-r/
# http://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
# http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
