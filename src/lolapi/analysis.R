library('cluster')  # for clusplot
library('scatterplot3d')  # for scatterplot3d
source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('data/data.csv')
# > nrow(data)
# [1] 85470

# Data features
features = names(data)
features.info = features[1:4]
features.boolean = features[5:7]
features.integer = features[8:25]

# ---------------------
# Treatment of outliers
# ---------------------

# Boxplot to analyze the outliers of all integer features. Boolean features do
# not need be analyzed.
save.boxplot(
    data[, features.integer],
    main='[Outlier] All integer features boxplot 1',
    names=range(len(features.integer))
)

# As we can see from the above plot that some features has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
for (feature in features.integer) {
    save.plot(data[, feature], main=strf('[Outlier] %s plot 1', feature))
    save.boxplot(data[, feature], main=strf('[Outlier] %s boxplot 1', feature))
}

# Automatically finding the lower and upper extreme outlier thresholds
# for each integer feature. IQR factor = 3.
thresholds = outlier_thresholds(data[, features.integer], factor=3)
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

# Boolean vector to indicate which data point is an extreme outlier or not.
outliers = rowmap(
    function(point) is.outlier(point, thresholds['lower',], thresholds['upper',]),
    data
)

# Filtering entire data to remove extreme outliers
data = data[!outliers,]
# > nrow(data)
# [1] 74656

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent matches need to be removed.

# Number of participants by match ID
participants_by_match = counter(data$matchId)

# Matches (IDs) that do not contain all 10 participants
inconsistent_matches = names(participants_by_match[participants_by_match < 10])

# Removing inconsistent matches from data
data = data[!(data$matchId %in% inconsistent_matches),]
# > nrow(data)
# [1] 35140

# Plots to analyze the integer features after the treatments
save.boxplot(
    data[, features.integer],
    main='[Outlier] All integer features boxplot 2',
    names=range(len(features.integer))
)

for (feature in features.integer) {
    save.plot(data[, feature], main=strf('[Outlier] %s plot 2', feature))
    save.boxplot(data[, feature], main=strf('[Outlier] %s boxplot 2', feature))
}

# ----------------------------
# Data normalization (z-score)
# ----------------------------

# Since the features are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer features using Z-score. Boolean features do not need be normalized.
data.normalized = cbind(
    data[, features.boolean],
    scale(data[, features.integer])
)

# --------------------
# Correlation analysis
# --------------------

# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.
correlations = cor.mtest(data.normalized, method='spearman', exact=FALSE)

# Boxplot to analyze the correlation matrix. The absolute values are used,
# because the correlation direction does not matter in this case.
save.boxplot(
    abs(correlations$estimates),
    main='[Correlation] Features boxplot',
    names=range(ncol(correlations$estimates))
)

# Cluster dendrogram plot to analyze the affinity of each attribute based on
# the correlation matrix.
plot(
    hclust(dist(correlations$estimates)),
    main='[Correlation] Features Dendrogram'
) # https://rpubs.com/gaston/dendrograms

# Heatmap plot of the correlation matrix. p.values greater than significance
# level at 0.05 are indicated.
cor.plot(
    correlations$estimates,
    main='[Correlation] Features heatmap'
    p.mat=correlations$p.values,
    sig.level=0.05,
    method='number',
    order='alphabet'
) # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

# Correlation matrix features ranked by the mean of correlations for each one
features.ranked = cor.rank(abs(correlations$estimates))
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

# Based on correlation analysis, the following are redundant features and
# features with many correlation p.values greater than the significance level
features.unselect = c(
    'TotalDamageDealt',
    'TotalDamageDealtToChampions',
    'LargestMultiKill',
    'LargestKillingSpree',
    'FirstTowerAssist',
    'FirstBlood',
    'FirstTower'
)
# Explanation for redundants:
# TotalDamageDealt = PhysicalDamageDealt + MagicDamageDealt
# (GoldErned, TotalDamageDealtToChampions), TotalDamageDealt
# (Kills, LargestMultiKill), LargestKillingSpree

# Ranked feature selection.
features.selection = setdiff(features.ranked, features.unselect)
# [1] "GoldEarned"           "Kills"                "PhysicalDamageDealt"
# [4] "MinionsKilled"        "TotalDamageTaken"     "TowerKills"
# [7] "LargestCritStrike"    "NeutralMinionsKilled" "Assists"
# [10] "CrowdControl"         "MagicDamageDealt"     "WardsPlaced"
# [13] "TotalHealAmount"      "Deaths"

# Top 3 hightly correled features of the selection
features.topselection = features.selection[1:3]

# Reducing the dimensionality of the normalized data with selected features
data.reduced = data.normalized[, features.selection]

# ----------------
# K-means analysis
# ----------------

# To find the optimal k number of clusters we can use the method that finds the
# knee of the error curve, which tries to find an appropriate number of
# clusters analyzing the curve of a generated graph from a clustering (based on
# k-means) conducted for each possible.

# k-means clustering for each k = {1, ..., 50} number of clusters
fits = t(map(
    function(k) kmeans(data.reduced, k, algorithm='Lloyd', iter.max=200),
    range(50)
))

# Total within-cluster Sum of Square Error (SSE or SS) for each k-means fit
twss = rowmap(function(fit) fit$tot.withinss, fits)

# Plot to analyze the knee of error curve resultant of k-means fits
save.plot(twss, main='[K-means] Error curve', xlab='k', ylab='tot.withinss')

# Which is the optimal fit in this case for k = {5, ..., 9}? Let's analyse the
# between-cluster SSE rate (betweenss / totss) difference for each one.

# Between-cluster SSE rate differences for each k = {5, ..., 9} k-means fit
bssrd = map(
    function(k) betweenss.rate(fits[k,]) - betweenss.rate(fits[k - 1,]),
    range(5, 9)
)

# Plot to visualuze the between-cluster SSE rate differences
save.plot(
    range(5, 9),
    bssrd,
    main='[K-means] Between-cluster SSE rate differences',
    xlab='k fit'
)

# Analysing the between-cluster SSE rate differences, the k = 8 fit seems to
# have the best trade-off, as the rate difference does not vary so much after
# it. Let's add some extra components and save it.
fit = fits[8,]

# Between-cluster SSE rate
fit$betweenss.rate = betweenss.rate(fit)

# Number of clusters
fit$k = len(fit$size)

# Variance for each cluster
fit$withinvar = 1 / (fit$size - 1) * fit$withinss

# Saving all components
for (component in names(fit))
    write.csv(fit[[component]], strf('data/fit/%s.csv', component))

# --------------------------
# Analysis with labeled data
# --------------------------

# Associating each reduced data point with its info and label features
labeled = cbind(data[, features.info], label=fit$cluster, data.reduced)

# Spliting labeled data between winners and losers
winners = labeled[labeled$Win == 1,]
losers = labeled[labeled$Win == 0,]

# Clusplot analysis
# -----------------

# A sample with n = 80 random points from labeled data
data.sampled = labeled[sample(range(nrow(labeled)), 80),]

# Clusplot of sampled data
clusplot(
    data.sampled[, features.selection],
    data.sampled$label,
    labels=4,
    col.clus= sort(unique(data.sampled$label)),
    col.p=data.sampled$label,
    lines=0
)

# Scatter plot analysis
# ---------------------

# Plot of labeled data
plot(labeled[, features.selection], col=labeled$label)

# Only the top correlated features
plot(labeled[, features.topselection, col=labeled$label)

# Plot of the top correlated features for each split
plot(winners[, features.topselection], col=winners$label)
plot(losers[, features.topselection], col=losers$label)

# Principal Component Analysis (PCA)
------------------------------------

# PCA of labeled data
labeled.pca = prcomp(labeled[, features.selection], center=TRUE)

# PCA of winners split
winners.pca = prcomp(winners[, features.selection], center=TRUE)

# PCA of losers split
losers.pca = prcomp(losers[, features.selection], center=TRUE)

# Selecting principal components to view
pca_indices = range(3)

# 3-D visualization of principal components of the labeled data
scatterplot3d(labeled.pca$x[, pca_indices], color=labeled$label, angle=95)

# 3-D visualization of principal components of winners
scatterplot3d(winners.pca$x[, pca_indices], color=winners$label, angle=95)

# 3-D visualization of principal components of losers
scatterplot3d(losers.pca$x[, pca_indices], color=losers$label, angle=95)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is splited between winners and losers.

# ----------
# Hypothesis
# ----------

# H1-0: Não existe diferença entre as distribuições dos clusters encontrados

kruskal.test(rowSums(labeled[, features.selection]), labeled$label)
# Kruskal-Wallis rank sum test
# Kruskal-Wallis chi-squared = 30223, df = 7, p-value < 2.2e-16

# Não existe diferença entre a quantidade de pontos dos clusters
wilcox.test(counter(labeled$label), conf.int=T)
# Wilcoxon signed rank test
# V = 36, p-value = 0.007813
# alternative hypothesis: true location is not equal to 0
# 95 percent confidence interval:
# 49032.26 73588.02
# sample estimates:
# (pseudo)median
#      62429.24

# H2-0: Não existe diferença entre as medianas dos jogadores vitoriosos e perdedores
x = rowSums(winners[, features.selection])
y = rowSums(losers[, features.selection])
wilcox.test(x , y, paired=FALSE)


# H2'-0: Para cada cluster encontrado, não existe diferença entre as medianas
# dos jogadores vitoriosos e perdedores

x = rowSums(winners[winners$label == 1, features.selection])
y = rowSums(losers[ losers$label == 1, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 3452800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 2, features.selection])
y = rowSums(losers[ losers$label == 2, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 399230, p-value = 0.02427
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 3, features.selection])
y = rowSums(losers[ losers$label == 3, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 6572200, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 4, features.selection])
y = rowSums(losers[ losers$label == 4, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1983800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 5, features.selection])
y = rowSums(losers[ losers$label == 5, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 5152600, p-value = 5.074e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 6, features.selection])
y = rowSums(losers[ losers$label == 6, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1006300, p-value = 1.915e-10
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 7, features.selection])
y = rowSums(losers[ losers$label == 7, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 828060, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(winners[ winners$label == 8, features.selection])
y = rowSums(losers[ losers$label == 8, features.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 2654500, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
