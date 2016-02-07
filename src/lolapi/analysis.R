library("car")
library(cluster)  # for clusplot
library("scatterplot3d")  # scatterplot3d
source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

data = read.csv("data/ranked_matches_2015_ong_features.csv")
# > nrow(data)
# [1] 85470

# Data attributes
data.attrs = names(data)
data.attrs.info = data.attrs[1:4]
data.attrs.boolean = data.attrs[5:7]
data.attrs.integer = data.attrs[8:25]
data.attrs.numerical = c(data.attrs.boolean, data.attrs.integer)

# ---------------------
# Treatment of outliers
# ---------------------

# Boxplot to analyze the outliers of all integer attributes. Boolean attributes
# do not need be analyzed.
save.boxplot(
    data[, data.attrs.integer],
    main='[Outlier] All integer attributes boxplot',
    names=range(len(data.attrs.integer))
)

# As we can see from the above plot that some attributes has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
for (attr in data.attrs.integer) {
    save.plot(data[, attr], main=strf('[Outlier] %s scatterplot', attr))
    save.boxplot(data[, attr], main=strf('[Outlier] %s boxplot', attr))
}

# Automatically finding the lower and upper extreme outlier thresholds
# (IQR factor = 3) for each integer attribute
thresholds = outlier_thresholds(data[, data.attrs.integer], factor=3)
# > t(thresholds)
#                                  lower    upper
# Kills                           -19.00     30.0
# Assists                         -19.00     37.0
# Deaths                          -11.00     24.0
# GoldEarned                    -7465.00  29957.0
# TotalDamageDealt            -229216.25 433109.8
# MagicDamageDealt            -112411.75 169991.0
# PhysicalDamageDealt         -260943.50 380499.8
# TotalDamageDealtToChampions  -33290.00  66089.0
# TotalDamageTaken             -25844.25  69949.0
# MinionsKilled                  -347.00    556.0
# NeutralMinionsKilled            -59.00     81.0
# CrowdControl                  -1023.00   1567.0
# WardsPlaced                     -17.00     32.0
# TowerKills                       -3.00      4.0
# LargestMultiKill                 -2.00      5.0
# LargestKillingSpree             -12.00     16.0
# LargestCritStrike             -1653.00   2204.0
# TotalHealAmount               -7896.00  11865.0

# Boolean vector to indicate which data point x is an extreme outlier or not.
outliers = apply(data, 1, function(x) is.outlier(x, thresholds))

# Filtering entire data to remove extreme outliers
data = data[!outliers,]
# > nrow(data)
# [1] 74656

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent matches need to be removed.

# Number of participants by match id
participants_by_match = counter(data$matchId)

# Matches (IDs) that do not contain all 10 participants
inconsistent_matches = names(participants_by_match[participants_by_match < 10])

# Removing inconsistent matches from data
data = data[!(data$matchId %in% inconsistent_matches),]
# > nrow(data)
# [1] 35140

# Plots to analyze the integer attributes after the treatments
save.boxplot(
    data[, data.attrs.integer],
    main='[Outlier] All integer attributes - after',
    names=range(len(data.attrs.integer))
)

for (attr in data.attrs.integer) {
    save.plot(data[, attr], main=strf('[Outlier] %s scatterplot - after', attr))
    save.boxplot(data[, attr], main=strf('[Outlier] %s boxplot - after', attr))
}

# ----------------------------
# Data normalization (z-score)
# ----------------------------

# Since the data attributes are of different varieties their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer attributes using Z-score. Boolean attributes do not need be
# normalized.
data.normalized = cbind(
    data[, data.attrs.boolean],
    scale(data[, data.attrs.integer])
)

# --------------------
# Correlation analysis
# --------------------

# Correlation matrix of normalized data attributes
correlations = cor(data.normalized)
correlations = abs(correlations)  # the signal does not matter
diag(correlations) = NA  # correlation of a set with itself does not matter

# Boxplot to analyze attributes correlation
save.boxplot(
    correlations,
    main='[Correlation] Bolean and integer attributes',
    names=range(ncol(correlations))
)

# Correlation matrix attributes ranked by the mean of correlations for each one
data.attrs.ranked = cor.rank(correlations)
# [1] "GoldEarned"                  "TotalDamageDealt"
# [3] "TotalDamageDealtToChampions" "Kills"
# [5] "PhysicalDamageDealt"         "MinionsKilled"
# [7] "LargestKillingSpree"         "LargestMultiKill"
# [9] "LargestCritStrike"           "TowerKills"
# [11] "TotalDamageTaken"            "Assists"
# [13] "MagicDamageDealt"            "CrowdControl"
# [15] "WardsPlaced"                 "NeutralMinionsKilled"
# [17] "Deaths"                      "FirstTower"
# [19] "TotalHealAmount"             "FirstBlood"
# [21] "FirstTowerAssist"

# ------------------------
# Dimensionality reduction
# ------------------------

# Automatic attribute selection based on the correlation matrix
data.attrs.selection = attribute_selection(correlations)
# [1] "Kills"                       "GoldEarned"
# [3] "TotalDamageDealt"            "PhysicalDamageDealt"
# [5] "TotalDamageDealtToChampions" "TotalDamageTaken"
# [7] "MinionsKilled"               "WardsPlaced"
# [9] "TowerKills"                  "LargestMultiKill"
# [11] "LargestKillingSpree"         "LargestCritStrike"

# Selection with attributes ranked
data.attrs.rankedselection = intersect(data.attrs.ranked, data.attrs.selection)

# Top 3 most correled attributes of the selection
data.attrs.topselection = data.attrs.rankedselection[1:3]

# Reducing the dimensionality of the normalized data with selected attributes
data.reduced = data.normalized[, data.attrs.selection]

# ----------------
# K-means analysis
# ----------------

# To find the optimal k number of clusters we can use the method that finds the
# knee of the error curve, which tries to find an appropriate number of
# clusters analyzing the curve of a generated graph from a test (based on
# k-means) conducted for each possible.

# Total within sum of squares of clusters for each k-means test k <= 20
twss = map(
    function(k) kmeans(data.reduced, k, algorithm='Lloyd')$tot.withinss,
    range(50)
)

# Plot to analyze the knee of error curve resultant of the test for each k
save.plot(twss, main="[K-means] Error curve", xlab="k", ylab="tot.withinss")

# Which is the optimal k value in this case? k={4, 5, 6, 7, 8, 9}?
# Let's analyse using the sum of square rate decreased from the total after
# partitioning the data into k clusters:
# total_SS = sum(ss(data))
# tot.withinss = sum(k-means(data, k)$withinss)
# between_SS = tot.withinss - total_SS
# between_SS / totalss

fit0 = kmeans(data.reduced, 4, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  48.3 %)

fit1 = kmeans(data.reduced, 5, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  53.5 %)
# fit1 - fit0 = 5.2 %

fit2 = kmeans(data.reduced, 6, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  56.7 %)
# fit2 - fit1 = 3.2 %

fit3 = kmeans(data.reduced, 7, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  59.1 %)
# fit3 - fit2 = 2.4 %

fit4 = kmeans(data.reduced, 8, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  60.7 %)
# fit4 - fit3 = 1.6 %

fit5 = kmeans(data.reduced, 9, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  62.2 %)
# fit5 - fit4 = 1.5 %

# fit4 has the best trade-off:
plot(c(4:8), c(5.2, 3.2, 2.4, 1.6, 1.5), xlab='fit', ylab='fit[i] - fit[i-1]')

# Let's add some extra components to fit4, then save it.

# Variance for each cluster of fit4
fit4$withinvar = 1 / (fit4$size - 1) * fit4$withinss

# Number of clusters of fit4
fit4$k = len(fit4$size)

# Saving all components of fit4
for (component in names(fit4))
    write.csv(fit4[[component]], strf('data/fit4/%s.csv', component))

# --------------------------------------------------
# Analysis of the data labeled by k-means clustering
# --------------------------------------------------

# Associating each reduced tuple with its info, win and label attributes
data.labeled = cbind(data[, data.attrs.info], label=fit4$cluster, data.reduced)

# Spliting labeled data between winners and losers
vencedores = data.labeled[data.labeled$Win == 1,]
perdedores = data.labeled[data.labeled$Win == 0,]

# Clusplot analysis
# -----------------

# A sample with n=80 random rows from labeled data
data.sampled = data.labeled[sample(range(nrow(data.labeled)), 80),]

# Clusplot of clusterized data (n=80)
clusplot(
    data.sampled[, data.attrs.selection],
    data.sampled$label,
    labels=4,
    col.clus= sort(unique(data.sampled$label)),
    col.p=data.sampled$label,
    lines=0
)

# Scatter plot analysis
# ---------------------

# Plot of the labeled data
plot(data.labeled[, data.attrs.selection], col=data.labeled$label)

# Only top correlated attributes
plot(data.labeled[, data.attrs.topselection], col=data.labeled$label)

# Scatterplot of most correlated attributes for each split
plot(vencedores[, data.attrs.topselection], col=vencedores$label)
plot(perdedores[, data.attrs.topselection], col=perdedores$label)

# Principal Component Analysis (PCA)
------------------------------------

# PCA of labeled data
data.labeled.pca = prcomp(data.labeled[, data.attrs.selection], center=TRUE)

# PCA of winners
vencedores.pca = prcomp(vencedores[, data.attrs.selection], center=TRUE)

# PCA of losers
perdedores.pca = prcomp(perdedores[, data.attrs.selection], center=TRUE)

# Principal components to view
pca_indices = c(1, 2, 3)

# 3-D visualization of principal components of the labeled data
scatterplot3d(
    data.labeled.pca$x[, pca_indices],
    pch=data.labeled$label,
    type="h",
    angle=95,  # 30
    color=data.labeled$label
)

# 3-D visualization of principal components of winners
scatterplot3d(
    vencedores.pca$x[, pca_indices],
    pch=vencedores$label,
    type="h",
    angle=95,
    color=vencedores$label
)

# 3-D visualization of principal components of losers
scatterplot3d(
    perdedores.pca$x[, pca_indices],
    pch=perdedores$label,
    type="h",
    angle=95,
    color=perdedores$label
)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is splited between winners and losers.

# ----------
# Hypothesis
# ----------

# H1-0: Não existe diferença entre as distribuições dos clusters encontrados no
# modelo de aprendizagem

# dados normalizados z-score
kruskal.test(rowSums(data.reduced), fit4$cluster)
# Kruskal-Wallis rank sum test
# Kruskal-Wallis chi-squared = 30223, df = 7, p-value < 2.2e-16

# analisando o tamanho de cada cluster: não existe diferença?
wilcox.test(fit4$size, conf.int=T)
# Wilcoxon signed rank test
# V = 36, p-value = 0.007813
# alternative hypothesis: true location is not equal to 0
# 95 percent confidence interval:
# 49032.26 73588.02
# sample estimates:
# (pseudo)median
#      62429.24

# H2-0: Para cada cluster encontrado, não existe diferença entre as medianas
# dos jogadores vitoriosos e perdedores

x = rowSums(vencedores[ vencedores$label == 1, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 1, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 3452800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 2, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 2, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 399230, p-value = 0.02427
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 3, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 3, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 6572200, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 4, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 4, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1983800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 5, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 5, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 5152600, p-value = 5.074e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 6, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 6, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1006300, p-value = 1.915e-10
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 7, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 7, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 828060, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 8, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 8, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 2654500, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# References
# [Kardi Teknomo] Kardi Teknomo. ANALYTIC HIERARCHY PROCESS (AHP) TUTORIAL. https://docs.google.com/file/d/0BxYU82vErc8xYS1PendBeHlpSkk/edit?usp=sharing
# http://stats.stackexchange.com/questions/49521/how-to-find-variance-between-multidimensional-points
# http://www.edureka.co/blog/clustering-on-bank-data-using-r/
# http://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
# http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
# http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
# http://blog.melchua.com/2012/09/07/r-basics-accessing-elements-of-an-object/
# https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusplot.default.html
