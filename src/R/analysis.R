source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('../data/data.csv')
# > nrow(data)
# [1] 85470

# Data features
features = names(data)
features.info = features[1:5]
# [1] "matchId"       "matchCreation" "summonerId"    "championId"
# [5] "winner"
features.boolean = features[6:8]
# [1] "firstBloodKill"   "firstTowerKill"   "firstTowerAssist"
features.integer = features[9:26]
# [1] "kills"                       "assists"
# [3] "deaths"                      "goldEarned"
# [5] "totalDamageDealt"            "magicDamageDealt"
# [7] "physicalDamageDealt"         "totalDamageDealtToChampions"
# [9] "totalDamageTaken"            "minionsKilled"
# [11] "neutralMinionsKilled"        "totalTimeCrowdControlDealt"
# [13] "wardsPlaced"                 "towerKills"
# [15] "largestMultiKill"            "largestKillingSpree"
# [17] "largestCriticalStrike"       "totalHeal"

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
# lower    upper
# kills                           -19.00     30.0
# assists                         -19.00     37.0
# deaths                          -11.00     24.0
# goldEarned                    -7465.00  29957.0
# totalDamageDealt            -229216.25 433109.8
# magicDamageDealt            -112411.75 169991.0
# physicalDamageDealt         -260943.50 380499.8
# totalDamageDealtToChampions  -33290.00  66089.0
# totalDamageTaken             -25844.25  69949.0
# minionsKilled                  -347.00    556.0
# neutralMinionsKilled            -59.00     81.0
# totalTimeCrowdControlDealt    -1023.00   1567.0
# wardsPlaced                     -17.00     32.0
# towerKills                       -3.00      4.0
# largestMultiKill                 -2.00      5.0
# largestKillingSpree             -12.00     16.0
# largestCriticalStrike         -1653.00   2204.0
# totalHeal                     -7896.00  11865.0

# Boolean vector to indicate which data point p is an extreme outlier or not.
outliers = rowmap(
    function(p) is.outlier(p, thresholds['lower',], thresholds['upper',]),
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
# https://rpubs.com/gaston/dendrograms
save.plot(
    hclust(dist(correlations$estimates)),
    main='[Correlation] Features Dendrogram'
)

# Heatmap plot of the correlation matrix. p.values greater than significance
# level at 0.05 are indicated.
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
save.cor.plot(
    correlations$estimates,
    main='[Correlation] Features heatmap',
    p.mat=correlations$p.values,
    sig.level=0.05,
    method='number',
    order='alphabet'
)

# Correlation matrix features ranked by the mean of correlations for each one
features.ranked = cor.rank(abs(correlations$estimates))
# [1] "goldEarned"                  "totalDamageDealt"
# [3] "totalDamageDealtToChampions" "kills"
# [5] "physicalDamageDealt"         "largestKillingSpree"
# [7] "minionsKilled"               "largestMultiKill"
# [9] "totalDamageTaken"            "towerKills"
# [11] "largestCriticalStrike"       "neutralMinionsKilled"
# [13] "assists"                     "totalTimeCrowdControlDealt"
# [15] "magicDamageDealt"            "wardsPlaced"
# [17] "totalHeal"                   "deaths"
# [19] "firstTowerKill"              "firstBloodKill"
# [21] "firstTowerAssist"

# ------------------------
# Dimensionality reduction
# ------------------------

# The features with high affinity (dendogram plot) and high correlation
# (heatmap plot) > 0.7 are redundants. The following are redundant features and
# features with many correlation p.values greater than the significance level.
features.unselect = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions',  # (goldErned, totalDamageDealtToChampions)
    'largestMultiKill',  # (Kills, LargestMultiKill)
    'largestKillingSpree',  # (Kills, LargestMultiKill), LargestKillingSpree
    'firstTowerAssist',  # 5 correlations > sig. level
    'firstBloodKill',  # 4 correlations > sig. level
    'firstTowerKill'  #  2 correlations > sig. level
)

# Ranked feature selection.
features.selection = setdiff(features.ranked, features.unselect)
# [1] "goldEarned"                 "kills"
# [3] "physicalDamageDealt"        "minionsKilled"
# [5] "totalDamageTaken"           "towerKills"
# [7] "largestCriticalStrike"      "neutralMinionsKilled"
# [9] "assists"                    "totalTimeCrowdControlDealt"
# [11] "magicDamageDealt"           "wardsPlaced"
# [13] "totalHeal"                  "deaths"

# Top 3 hightly correled features of the selection
features.topselection = features.selection[1:3]

# Reducing the dimensionality of the normalized data with selected features
data.reduced = data.normalized[, features.selection]

# ------------------------
# Learning model (K-means)
# ------------------------

# K-means clustering model aims to partition the data into k clusters, so as to
# minimize the sum of squared error (SSE or SS). To find the optimal k we can
# use the the knee of the error curve method, which tries to find an appropriate
# number of clusters analyzing the curve of a generated graph from a clustering
# conducted for each possible.

# K-means clustering model/fit for each k = {1, ..., 50} number of clusters
fits = t(map(
    function(k) kmeans(data.reduced, k, algorithm='Lloyd', iter.max=200),
    range(50)
))

# Total within-cluster SSE for each k-means fit
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
    write.csv(fit[[component]], strf('../data/fit/%s.csv', component))

# Associating each reduced data point with its info and label features
labeled = cbind(data[, features.info], label=fit$cluster, data.reduced)

# Spliting labeled data between winners and losers
winners = labeled[labeled$winner == 1,]
losers = labeled[labeled$winner == 0,]

# ----------------------------------------------------
# Statistical analysis of the results (Non-parametric)
# ----------------------------------------------------

# Hypothesis 1. H1-0: There is no difference between the distributions of the
# clusters found in the learning model; H1-1 There is difference between the
# distributions of the clusters found in the learning model. Test:
# Kruskal-Wallis rank sum test

kruskal.test(rowSums(labeled[, features.selection]), labeled$label)
# Alternative hypothesis true: p.value < 0.05

# Hypothesis 2. H2-0: For each cluster found in the learning model there is no
# difference between the medians of the winning players and losing players;
# (H2-1) for each cluster found there is difference between the medians of the
# winning players and losing players. Test: Wilcoxon rank sum test with
# continuity correction

x = rowSums(winners[winners$label == 1, features.selection])
y = rowSums(losers[ losers$label == 1, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 2, features.selection])
y = rowSums(losers[ losers$label == 2, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 3, features.selection])
y = rowSums(losers[ losers$label == 3, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 4, features.selection])
y = rowSums(losers[ losers$label == 4, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 5, features.selection])
y = rowSums(losers[ losers$label == 5, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 6, features.selection])
y = rowSums(losers[ losers$label == 6, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 7, features.selection])
y = rowSums(losers[ losers$label == 7, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

x = rowSums(winners[ winners$label == 8, features.selection])
y = rowSums(losers[ losers$label == 8, features.selection])
wilcox.test(x , y, paired=FALSE)
# Alternative hypothesis true: p.value < 0.05

# --------------------------
# Labeled data visualization
# --------------------------

# Clusplot analysis
# -----------------

# A sample with n = 80 random points from labeled data
data.sampled = labeled[sample(range(nrow(labeled)), 80),]

# Clusplot of sampled data
save.clusplot(
    data.sampled[, features.selection],
    data.sampled$label,
    main='Cluster plot of the labeled data (n=80)',
    labels=4,
    col.clus= sort(unique(data.sampled$label)),
    col.p=data.sampled$label,
    lines=0
)

# Scatter plot analysis
# ---------------------

# Plot of labeled data. Only the top selected features
save.plot(
    labeled[, features.topselection],
    main='[Visualization] Scatter plot',
    col=labeled$label
)

# Only winners
save.plot(
    winners[, features.topselection],
    main='[Visualization] Scatter plot - winners',
    col=winners$label
)

# Only losers
save.plot(
    losers[, features.topselection],
    main='[Visualization] Scatter plot - losers',
    col=losers$label
)

# Principal Component Analysis (PCA)
# ------------------------------------

# PCA of labeled data
labeled.pca = prcomp(labeled[, features.selection], center=TRUE)

# PCA of winners
winners.pca = prcomp(winners[, features.selection], center=TRUE)

# PCA of losers
losers.pca = prcomp(losers[, features.selection], center=TRUE)

# Selecting principal components to view
pca_indices = range(3)

# 3-D visualization of principal components of the labeled data
save.scatterplot3d(
    labeled.pca$x[, pca_indices],
    main='[Visualization] PCA',
    color=labeled$label,
    angle=95
)

# 3-D visualization of principal components of winners
save.scatterplot3d(
    winners.pca$x[, pca_indices],
    main='[Visualization] PCA - winners',
    color=winners$label,
    angle=95
)

# 3-D visualization of principal components of losers
save.scatterplot3d(
    losers.pca$x[, pca_indices],
    main='[Visualization] PCA - losers',
    color=losers$label,
    angle=95
)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is splited between winners and losers.
