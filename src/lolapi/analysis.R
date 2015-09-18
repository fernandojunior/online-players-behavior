
library("car")

options(scipen=999)

data = read.csv("data/ranked_matches_2015_ong_features.csv")

# Outliers analysis
# -----------------

# selecting only numerical data
boxplot(data[, 4:25]) # boolean and integer data
boxplot(data[, 8:25]) # only integer data

# As we can see from the above plots that some attributes has outliers in the data
# Let’s analyze all them individually.

# Function to analyze the outliers of a data set with boxplot and scatter plot
# title: Title of the analysis
# data: The data set
# scatterplot: If true, creates a scatter plot of the data and saves into a file
# names: If true, sets the boxplot names
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

# removing participants with 'large' outliers
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

# Apos a remocao dos participantes com large outilies
# alguns matches ficaram com menos de 10 participantes ...
# Essa funcao retorna matches que nao tem todos os participantes (<10) ...
matchs_without_participants = function () {

    # Funcao que verifica se existe um objeto x em uma lista l
    contains = function (x, l) {
        for (i in l)
            if (x == i)
			    return(TRUE)
        return(FALSE)
    }

	# list of matches without all participants
	to_be_removed = c()

	# list of match ids already verified
	already_verified = c()

	for(i in c(1:nrow(data))){

	    participant = data[i,]
	    matchId = participant$matchId

	    if(!contains(matchId, already_verified)) {

	    	# merge
	    	already_verified = c(already_verified, matchId)

	    	# verifying number of participants in match
		    total_participants = nrow(data[data$matchId == matchId,])

		    if (total_participants != 10)
		    	to_be_removed = c(to_be_removed, matchId)

	    }

	}

	return(to_be_removed)
	
}

# removendo matches que nao tem todos os 10 participantes
data = data[!(data$matchId %in% matchs_without_participants()), ]

# persisting data
write.csv(data, file = "data/ranked_matches_2015_no_largeoutliers.csv")

# Since the data attributes are of different varieties their scales are also different. 
# In order to maintain uniform scalability we scale the columns.
# normalizacao os dados numericos 8:25
data.normalized = cbind(data[,1:3], data[,4:7], scale(data[,8:25]))

# correlacoes dos atributos
correlations = cor(data.normalized[,4:25])
correlations.rounded = round(correlations, digits=1)
correlations.mod = sqrt(correlations.rounded * correlations.rounded)
write.csv(correlations.mod, file = "analysis/correlations_filtered_mod.csv", sep =",")

# dados tratados (sem diagonal e header) das correlacoes para o boxplot
correlations.boxplot = read.csv('analysis/correlations_filtered_mod_boxplot.csv', header=FALSE)

# boxplot das correlacoes
analysis.outliers('Correlações de atributos', correlations.boxplot, FALSE, TRUE) # apenas numericos

# apenas numericos: booleans + integers
data.reduzido = data.normalized[,4:25] 
# apenas atributos numericos com base na analise de correlacoes
data.reduzido = cbind(data.reduzido[,c(1,5)], data.reduzido[,7:14], data.reduzido[,17:21])

# removendo atr. win, pois eh boolean
data.reduzido = data.reduzido[,2:ncol(data.reduzido)]

# renomeando para melhor manipulacao
ldata = data.reduzido

# Calculating total sum of squares and storing at the first index in wss
wss <- (nrow(ldata)-1)*sum(apply(ldata,2,var))

# max value of k
max = 50

# Calculating the sum of squares for each k == i and storing in wss[i]
for(i in 2 : max)
    wss[i] <- sum(fit = kmeans(ldata, centers = i, max, algorithm = 'Lloyd')$withinss)

# Analysing the 'knees' of the plot to find the ideal k number of cluster
plot(
    1:max,
    wss,
    type = "b", 
    main = "k clusters", 
    xlab = "no. of cluster", 
    ylab = "within cluster sum of squares")

fit <- kmeans(ldata, 11, algorithm='Lloyd')

library(cluster)

fit <- kmeans(ldata, 4, algorithm='Lloyd')
clusplot(ldata[1:80,], fit$cluster[1:80], color=FALSE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(ldata[1:200,],4)
clusplot(ldata[1:80,], fit$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(data.numerical.scaled, 4, algorithm='Lloyd')


library("scatterplot3d")

scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(1,2,3)], pch = fit$cluster, type = "h", angle = 55, color = fit$cluster)

scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(1,2,3)], pch = fit$cluster, type = "h", angle = 95, color = fit$cluster)

# most cor: goldEarned, totalDamageDealt, minionsKilled
scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(3,4,9)], pch = fit$cluster, type = "h", angle = 95, color = fit$cluster)

# dispersao com cluster
plot(ldata,col=fit$cluster,pch=15)

# dispersao apenas mais correlacionados
plot(ldata[,c(3,4,9)],col=fit$cluster,pch=15)

# merge com dados nominais sobre os participantes, stats, e cluster
ldata2 = cbind(data[, c(1,2,3,4)], ldata, cluster)

# dispersao com cluster dos participantes perdedores
perdedores = ldata2[ldata2$Win == 0,5:(ncol(ldata2))]
plot(perdedores[,c(3,4,9)],col=perdedores$cluster,pch=15)

# dispersao com cluster dos participantes perdedores
vencedores = ldata2[ldata2$Win == 1,5:(ncol(ldata2))]
plot(vencedores[,c(3,4,9)],col=vencedores$cluster,pch=15)

# scatterplot most cor dos perdedores
scatterplot3d(prcomp(perdedores, center = TRUE)$x[,c(3,4,9)], pch = perdedores$cluster, type = "h", angle = 95, color = perdedores$cluster)

# corrigido: apenas inteiros ... attr cluster nao considerado
scatterplot3d(prcomp(perdedores[,1:(ncol(perdedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = perdedores$cluster, type = "h", angle = 95, color = perdedores$cluster)


scatterplot3d(prcomp(vencedores, center = TRUE)$x[,c(3,4,9)], pch = vencedores$cluster, type = "h", angle = 95, color = vencedores$cluster)

# corrigido: apenas inteiros ... attr cluster nao considerado
scatterplot3d(prcomp(vencedores[,1:(ncol(vencedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = vencedores$cluster, type = "h", angle = 95, color = vencedores$cluster)

# diferentes k para analise
fit = kmeans(ldata, 11, algorithm='Lloyd')
fit2 = kmeans(ldata, 11, algorithm='Lloyd', iter.max=150)
fit3 = kmeans(ldata, 8, algorithm='Lloyd')
fit4 = kmeans(ldata, 8, algorithm='Lloyd', iter.max=150)

# gravando fits para uso posterior
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

$ fit = kmeans(ldata, 11, algorithm='Lloyd')
 (between_SS / total_SS =  61.2 %)
Warning: did *not* converge in specified number of iterations

$ fit2 =  kmeans(ldata, 11, algorithm='Lloyd', iter.max=150)
 (between_SS / total_SS =  61.9 %)

$ fit3 = kmeans(ldata, 8, algorithm='Lloyd')
 (between_SS / total_SS =  57.2 %)
Warning: did *not* converge in specified number of iterations

$ fit4 = kmeans(ldata, 8, algorithm='Lloyd', iter.max=150)
 (between_SS / total_SS =  57.7 %)

clusplot(ldata[1:80,], fit4$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)
legend("bottomleft", legend = paste("Group", 1:8), pch=1, col=1:8)

# dispersao com cluster
plot(ldata,col=fit4$cluster,pch=15)

# dispersao apenas mais correlacionados
plot(ldata[,c(3,4,9)],col=fit4$cluster,pch=15)


Cluster = fit4$cluster

ldata2 = cbind(data[, c(1,2,3,4)], ldata, Cluster)

perdedores = ldata2[ldata2$Win == 0, 5:(ncol(ldata2))]
plot(perdedores[,c(3,4,9)],col=perdedores$Cluster,pch=15)

# dispersao com cluster dos participantes perdedores
vencedores = ldata2[ldata2$Win == 1,5:(ncol(ldata2))]
plot(vencedores[,c(3,4,9)],col=vencedores$Cluster,pch=15)

library("scatterplot3d")

scatterplot3d(prcomp(ldata, center = TRUE)$x[,c(3,4,9)], pch = fit4$cluster, type = "h", angle = 95, color = fit4$cluster)

scatterplot3d(prcomp(perdedores[,1:(ncol(perdedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = perdedores$Cluster, type = "h", angle = 95, color = perdedores$Cluster)

scatterplot3d(prcomp(vencedores[,1:(ncol(vencedores)-1)], center = TRUE)$x[,c(3,4,9)], pch = vencedores$Cluster, type = "h", angle = 95, color = vencedores$Cluster)



# sum of squared cluster 1
sum(apply(classified_data[classified_data$Cluster==1,c(1:15)],2,var)) * (fit4$size[1] - 1)

# sum of squared hole data
wss = (nrow(ldata)-1) * sum(apply(mydata,2,var))

# within sum of squares
ss = function (input, cluster) {
	x = input[input$Cluster == cluster, ]
	s = nrow(x) # size
	v = sum(apply(x,2,var))  # variance
	ssd = ((s-1) * v) # within sum of square
	return(c(s, ssd, v)) # returns the values
}

ss.vencedores = c('size', 'sun of sq', 'var')

for(i in c(1:8)) {
	ss.vencedores = rbind(i=ss.vencedores, ss(vencedores, i))
}

#3739 41316.9371603666 11.0532202141163
#4281 19268.3432556076 4.50194935878682
#5223 49015.2318475736 9.38629487697695
#5179 15236.4693924323 2.94253947323915
#4035 27864.5755564661 6.90743072792912
#3691 18353.4704144465 4.97384022071721
#5835 46343.2471402998 7.94364880704487
#9667 52208.3269817618 5.40123391079679

ss.perdedores = c('size', 'sun of sq', 'var')

for(i in c(1:8)) {
	ss.perdedores = rbind(i=ss.perdedores, ss(perdedores, i))
}

#1674 16687.9878413122 9.97488812989375 
#3772 16583.1388273172 4.39754410695233 
#3217 25773.5785903237 8.01417244723994 
#11838 34719.6991407263 2.9331502188668  
#6453 39535.8716607882 6.12769244587541 
#10711 51691.7332768328 4.82649236945218 
#418 2547.04791512543 6.10802857344227 
#3567 14730.9541129319 4.13094618982947 

for(i in c(1:8)) {
	print(fit4$withinss[i] / (fit4$size[i]-1))
}

# h0: mean ==
aggregate(Kills ~ Cluster, perdedores, mean)


aggregate(. ~ Cluster, perdedores, mean) - aggregate(. ~ Cluster, ldata, mean)


### H1-0: Não existe diferença entre as distribuições dos clusters encontrados no modelo de aprendizagem

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
