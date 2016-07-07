# Draft and notes...

##

d = dist(data.numerical.scaled, method = "euclidean") # distance matrix

fit = hclust(d, method="ward")

plot(fit) # display dendogram

##

library(cluster)
library(fpc) # !!!!!!!!!

dat = data.numerical.scaled[,2:22]

# Kmeans clustre analysis
clus=kmeans(dat, centers=2)

# Fig 01
plotcluster(dat, clus$cluster)

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

# TODO Summarying partitions
----------------------------

# deprecated functions

sum_of_squares = function (X) {
    "Return sum of squares of multidimensional X sample data: (n-1) * Var(X)."
    n = nrow(X) # size
    VarX = colmap(var, x)  # variance
    SS = (n-1) * VarX # sum of square
    result = c()
    result$size = n
    result$ss = SS
    result$var = VarX
    return(result)
}

total_sum_of_squares = function (X) {
    "Return total sum of squares of multidimensional X sample data: sum(ss(X))."
    ss = sum_of_squares(X)
    result = c()
    result$size = ss$size
    result$tss = sum(ss$ss)
    result$tvar = sum(ss$var)
    return(result)
}

# Within cluster sum of squares
withinss = function (data, cluster) {
    X = data[data$Cluster == cluster, ]
    tss = total_sum_of_squares(X)
    return(c(tss$size, tss$tss, tss$tvar))
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
tmp = data[, data.attrs.selection]
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



# H1-0
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





endswith = function (s, suffix) {
    "Return TRUE if s ends with the specified suffix, FALSE otherwise."
    return(grepl(strf('%s$', suffix), s))
}

startswith = function (s, prefix) {
    "Return TRUE if s starts with the specified prefix, FALSE otherwise."
    return(grepl(strf('^%s', prefix), s))
}

# similar players dendrogram
kmeans(data.normalized, 8)
plot(
    hclust(dist(data.normalized[r, ])),
    main='Players Dendrogram',
    labels=data[sample(range(nrow(data.normalized)), 100), 'summonerId'],
    # col=kmeans(data.normalized, 8)$cluster[r]
)


# TODO barplot to compare clusters

# TODO function for outliers analysis

# TODO As the match duration varies between the matches, the features must be divided by match duration.

# TODO remove duplicated summonerIds and choose only a participant for each match

# TODO win rate analysis
# (counter(winners$label) - counter(losers$label)) / (counter(winners$label) + counter(losers$label))

# TODO Champ analisys
# sort(table(winners[, 'championId']), decreasing=TRUE)
# sort(table(winners[winners$label == 3, 'championId']), decreasing=TRUE)
