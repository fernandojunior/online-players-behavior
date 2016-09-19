source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('../data/data20160609012259.csv')
# nrow(data)
# [1] 85470

# Remove duplicated rows by summonerId to decrease bias
data = data[!duplicated(data[, 'summonerId']), ]
# [1] 54681

# features
features = names(data)

features.info = c(
    'championId', 'matchCreation', 'matchCreationDay', 'matchCreationHour',
    'matchCreationMonth', 'matchCreationYear', 'matchDuration', 'matchId',
    'matchMode', 'queueType', 'season', 'summonerId'
)

features.target = 'winner'

features.boolean = c(
    'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill'
)

# all participant stats
features.stats = c(
    'assists', 'champLevel', 'combatPlayerScore', 'deaths', 'doubleKills',
    'firstBloodAssist', 'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill', 'goldEarned',
    'goldSpent', 'inhibitorKills', 'item0', 'item1', 'item2', 'item3', 'item4',
    'item5', 'item6', 'killingSprees', 'kills', 'largestCriticalStrike',
    'largestKillingSpree', 'largestMultiKill', 'magicDamageDealt',
    'magicDamageDealtToChampions', 'magicDamageTaken', 'minionsKilled',
    'neutralMinionsKilled', 'neutralMinionsKilledEnemyJungle',
    'neutralMinionsKilledTeamJungle', 'objectivePlayerScore', 'pentaKills',
    'physicalDamageDealt', 'physicalDamageDealtToChampions',
    'physicalDamageTaken', 'quadraKills', 'sightWardsBoughtInGame',
    'totalDamageDealt', 'totalDamageDealtToChampions', 'totalDamageTaken',
    'totalHeal', 'totalPlayerScore', 'totalScoreRank',
    'totalTimeCrowdControlDealt', 'totalUnitsHealed', 'towerKills',
    'tripleKills', 'trueDamageDealt', 'trueDamageDealtToChampions',
    'trueDamageTaken', 'unrealKills', 'visionWardsBoughtInGame',
    'wardsKilled', 'wardsPlaced', 'winner'
)

feaures.unstats = c(
    # DOMINION matchMode features
    'combatPlayerScore', 'objectivePlayerScore', 'totalPlayerScore',
    'totalScoreRank', 'unrealKills',
    # categorical features
    'champLevel', 'item0', 'item1', 'item2', 'item3', 'item4', 'item5', 'item6',
    # boolean features
    'firstBloodAssist', 'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill',
    # target feature
    'winner'
)

# features to analyse: stats - unstats
features.numeric = setdiff(features.stats, feaures.unstats)

# ---------------------
# Treatment of outliers
# ---------------------

# Boxplot to analyze the outliers of all integer features. Boolean features do
# not need be analyzed.
save.boxplot(
    data[, features.numeric],
    main='[Outlier] All integer features boxplot 1',
    names=range(length(features.numeric))
)

# As we can see from the above plot that some features has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
# for (feature in features.numeric) {
#     save.plot(data[, feature], main=strf('[Outlier] %s plot 1', feature))
#     save.boxplot(data[, feature], main=strf('[Outlier] %s boxplot 1', feature))
# }

# Automatically finding the lower and upper extreme outlier thresholds
# for each integer feature. IQR factor = 3.
thresholds = outlier_thresholds(data[, features.numeric], factor=3)
# t(thresholds)
# assists                             -19     37
# deaths                              -11     24
# doubleKills                          -3      4
# goldEarned                        -7491  30008
# goldSpent                         -7420  27895
# inhibitorKills                        0      0
# killingSprees                        -6      8
# kills                               -19     30
# largestCriticalStrike             -1644   2192
# largestKillingSpree                 -12     16
# largestMultiKill                     -2      5
# magicDamageDealt                -110875 168117
# magicDamageDealtToChampions      -26558  39151
# magicDamageTaken                 -15235  30482
# minionsKilled                      -351    559
# neutralMinionsKilled                -59     81
# neutralMinionsKilledEnemyJungle      -9     12
# neutralMinionsKilledTeamJungle      -48     64
# pentaKills                            0      0
# physicalDamageDealt             -259807 378810
# physicalDamageDealtToChampions   -34075  49036
# physicalDamageTaken              -18829  45690
# quadraKills                           0      0
# sightWardsBoughtInGame               -3      4
# totalDamageDealt                -232569 435658
# totalDamageDealtToChampions      -33410  66123
# totalDamageTaken                 -26110  70511
# totalHeal                         -7933  11940
# totalTimeCrowdControlDealt        -1026   1571
# totalUnitsHealed                      1      1
# towerKills                           -3      4
# tripleKills                           0      0
# trueDamageDealt                  -15569  21020
# trueDamageDealtToChampions        -3132   4176
# trueDamageTaken                   -2193   3554
# visionWardsBoughtInGame              -3      4
# wardsKilled                          -3      4
# wardsPlaced                         -17     32

# select only features with different thresholds in order to mantain variability
thresholds.isdiff = diff(thresholds) != 0  # lower != upper
thresholds.features = names(thresholds.isdiff[, thresholds.isdiff == TRUE])
thresholds = thresholds[, thresholds.features]
features.numeric = thresholds.features
# features.numeric
# [1] "assists"                         "deaths"
# [3] "doubleKills"                     "goldEarned"
# [5] "goldSpent"                       "killingSprees"
# [7] "kills"                           "largestCriticalStrike"
# [9] "largestKillingSpree"             "largestMultiKill"
# [11] "magicDamageDealt"                "magicDamageDealtToChampions"
# [13] "magicDamageTaken"                "minionsKilled"
# [15] "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle"
# [17] "neutralMinionsKilledTeamJungle"  "physicalDamageDealt"
# [19] "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [21] "sightWardsBoughtInGame"          "totalDamageDealt"
# [23] "totalDamageDealtToChampions"     "totalDamageTaken"
# [25] "totalHeal"                       "totalTimeCrowdControlDealt"
# [27] "towerKills"                      "trueDamageDealt"
# [29] "trueDamageDealtToChampions"      "trueDamageTaken"
# [31] "visionWardsBoughtInGame"         "wardsKilled"
# [33] "wardsPlaced"

# Boolean vector to indicate which data point p is an extreme outlier or not.
outliers = rowmap(
    function(p) is.outlier(p, thresholds['lower',], thresholds['upper',]),
    data[, features.numeric]
)

# Filtering entire data to remove extreme outliers
data = data[!outliers,]
# nrow(data)
# [1] 40050

write.csv(data, "../data/treated.csv", row.names=FALSE)

# Plots to analyze the integer features after the treatments
save.boxplot(
    data[, features.numeric],
    main='[Outlier] All integer features boxplot 2',
    names=range(length(features.numeric))
)

# for (feature in features.numeric) {
#     save.plot(data[, feature], main=strf('[Outlier] %s plot 2', feature))
#     save.boxplot(data[, feature], main=strf('[Outlier] %s boxplot 2', feature))
# }

# ----------------------------
# Data normalization (z-score)
# ----------------------------

# Since the features are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer features using Z-score. Boolean features do not need be normalized.
# data.normalized = cbind(
#     data[, features.boolean],
#     scale(data[, features.numeric]/data[, 'matchDuration'])
# )

data.normalized = scale(data[, features.numeric]/data[, 'matchDuration'])

write.csv(data.normalized, "../data/normalized.csv", row.names=FALSE)

# --------------------
# Correlation analysis
# --------------------

# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.
correlations = cor.mtest(data.normalized, method='spearman', exact=FALSE)

write.csv(correlations$estimates, "../data/correlations.csv")

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

quit(save='no')

# Correlation matrix features ranked by the mean of correlations for each one
features.ranked = cor.rank(abs(correlations$estimates))
# [1] "totalDamageDealt"                "physicalDamageDealt"
# [3] "goldEarned"                      "goldSpent"
# [5] "kills"                           "physicalDamageDealtToChampions"
# [7] "totalDamageDealtToChampions"     "killingSprees"
# [9] "largestKillingSpree"             "minionsKilled"
# [11] "neutralMinionsKilled"            "doubleKills"
# [13] "largestCriticalStrike"           "neutralMinionsKilledEnemyJungle"
# [15] "largestMultiKill"                "trueDamageDealtToChampions"
# [17] "neutralMinionsKilledTeamJungle"  "towerKills"
# [19] "totalDamageTaken"                "magicDamageDealtToChampions"
# [21] "physicalDamageTaken"             "trueDamageDealt"
# [23] "wardsPlaced"                     "totalTimeCrowdControlDealt"
# [25] "assists"                         "magicDamageDealt"
# [27] "deaths"                          "sightWardsBoughtInGame"
# [29] "trueDamageTaken"                 "visionWardsBoughtInGame"
# [31] "magicDamageTaken"                "totalHeal"
# [33] "wardsKilled"

# ------------------------
# Dimensionality reduction
# ------------------------

# 1. quais pares sao mais similares?
# 2. dado um par, qual dos fatores tem menos corrrelacoes significativas?
# 3. sao iguais?
# 4. qual dos fatores tem mais alta correlacoes?

# The features with high affinity (dendogram plot) and high correlation
# (heatmap plot) > 0.7 are redundants. The following are redundant features and
# features with many correlation p.values greater than the significance level.
features.unselect = c(
    'doubleKills',  # (doubleKills, largestMultiKill)
    'goldSpent',  # (goldErned, goldSpent)
    'sightWardsBoughtInGame', 'visionWardsBoughtInGame',  # ('wardsPlaced' (sightWardsBoughtInGame, visionWardsBoughtInGame))
    'killingSpree', 'largestKillingSpree',  # (kills, (killingSpree, largestKillingSpree))
    'neutralMinionsKilled',  # (neutralMinionsKilled, neutralMinionsKilledTeamJungle)
    'physicalDamageDealt',  # (physicalDamageDealt, physicalDamageDealtToChampions); 4.
    'physicalDamageTaken',  # (physicalDamageTaken, totalDamageTaken); 2.
    'trueDamageDealt',  # (trueDamageDealtToChampions, (totalTimeCrowdControlDealt, trueDamageDealt))
    'magicDamageDealt',  # (magicDamageDealt, magicDamageDealtToChampions)
    'magicDamageTaken'  # (magicDamageTaken, trueDamageTaken); 2.
)

# Ranked feature selection.
features.selection = setdiff(features.ranked, features.unselect)
# [1] "totalDamageDealt"                "goldEarned"
# [3] "kills"                           "physicalDamageDealtToChampions"
# [5] "totalDamageDealtToChampions"     "killingSprees"
# [7] "minionsKilled"                   "largestCriticalStrike"
# [9] "neutralMinionsKilledEnemyJungle" "largestMultiKill"
# [11] "trueDamageDealtToChampions"      "neutralMinionsKilledTeamJungle"
# [13] "towerKills"                      "totalDamageTaken"
# [15] "magicDamageDealtToChampions"     "wardsPlaced"
# [17] "totalTimeCrowdControlDealt"      "assists"
# [19] "deaths"                          "trueDamageTaken"
# [21] "totalHeal"                       "wardsKilled"

# Top 3 hightly correled features of the selection
features.topselection = features.selection[1:3]
# [1] "totalDamageDealt" "goldEarned"       "kills"

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

# K-means clustering model/fit for each k = {1, ..., 20} number of clusters
fits = t(map(
    function(k) kmeans(data.reduced, k, algorithm='Lloyd', iter.max=200),
    range(1000)
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

# Analysing the between-cluster SSE rate differences, the k = 7 fit seems to
# have the best trade-off, as the rate difference does not vary so much after
# it. Let's add some extra components and save it.
fit = fits[7,]

# Between-cluster SSE rate
fit$betweenss.rate = betweenss.rate(fit)

# Number of clusters
fit$k = length(fit$size)

# Variance for each cluster
fit$withinvar = 1 / (fit$size - 1) * fit$withinss

# Saving all components
for (component in names(fit))
    write.csv(fit[[component]], strf('../output/fit/%s.csv', component))

write.csv(fit$cluster, "../data/cluster.csv", row.names=FALSE)

# Associating each reduced data point with its info and label features
labeled = cbind(data[, features.info], label=fit$cluster, data.reduced)
write.csv(labeled, "../data/labeled.csv", row.names=FALSE)

# treated_with_labels = cbind(data[, features.info], label=fit$cluster)
# write.csv(treated_with_labels, "treated_with_labels.csv", row.names=FALSE)

# Spliting labeled data between winners and losers
winners = labeled[labeled$winner == 1,]
losers = labeled[labeled$winner == 0,]

# -----------------------------------
# Statistical analysis of the results
# -----------------------------------

# Hypothesis 1. H1-0: There is no difference between the distributions of the
# clusters found in the learning model; H1-1 There is difference between the
# distributions of the clusters found in the learning model. Test:
# Kruskal-Wallis rank sum test

h1 = kruskal.test(rowSums(labeled[, features.selection]), labeled$label)

# Alternative hypothesis true: p.value < 0.05
save.plot(1, h1$p.value, main='[Hypothesis] H1', xlab='h1', ylab='p.value')

# Hypothesis 2. H2-0: For each cluster found in the learning model there is no
# difference between the medians of the winning players and losing players;
# (H2-1) for each cluster found there is difference between the medians of the
# winning players and losing players. Test: Wilcoxon rank sum test with
# continuity correction

h2.p.values = map(
    function (k) {
        x = rowSums(winners[winners$label == k, features.selection])
        y = rowSums(losers[ losers$label == k, features.selection])
        wilcox.test(x , y, paired=FALSE)$p.value
    },
    range(fit$k)
)

# Alternative hypothesis true for each cluster: p.value < 0.05
save.plot(h2.p.values, main='[Hypothesis] H2', xlab='k', ylab='p.values')

# ----------------------
# Exploring labeled data
# ----------------------

# Clusplot analysis
# -----------------

# A sample with n = 80 random points from labeled data
data.sampled = labeled[sample(range(nrow(labeled)), 80),]

# Clusplot of sampled data
save.clusplot(
    data.sampled[, features.selection],
    data.sampled$label,
    main='[Exploring] Cluster plot of the labeled data (n=80)',
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
    main='[Exploring] Scatter plot',
    col=labeled$label
)

# Only winners
save.plot(
    winners[, features.topselection],
    main='[Exploring] Scatter plot - winners',
    col=winners$label
)

# Only losers
save.plot(
    losers[, features.topselection],
    main='[Exploring] Scatter plot - losers',
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
    main='[Exploring] PCA',
    color=labeled$label,
    angle=95
)

# 3-D visualization of principal components of winners
save.scatterplot3d(
    winners.pca$x[, pca_indices],
    main='[Exploring] PCA - winners',
    color=winners$label,
    angle=95
)

# 3-D visualization of principal components of losers
save.scatterplot3d(
    losers.pca$x[, pca_indices],
    main='[Exploring] PCA - losers',
    color=losers$label,
    angle=95
)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is splited between winners and losers.

# Centroid analysis
# -----------------
# label = read.csv('../output/fit/cluster.csv')$x
# labeled = cbind(data[, features.info], label=label, data.reduced)
winners = labeled[labeled$winner == 1,]
losers = labeled[labeled$winner == 0,]

centers_by_label = function (x, features) {
    "Given a data set x, return the feature centers by label.

    The labeled feature centers are converted into a list, then joined by row.
    "
    label = x[, 'label']
    labels = sort(unique(label))
    centers = aggregate(. ~ label, x[, features], mean)
    feature_center_by_label = function(feature) {
        cbind(
            feature_id=indexof(feature, features),
            label=labels,
            center=centers[, feature]
        )
    }
    return(as.data.frame(do.call(
        rbind,
        lapply(setdiff(features, 'label'), feature_center_by_label)
    )))
}

plot_centers_by_label = function (x, features, main) {

    xlab = paste(sapply(
        range(length(features)),
        function(i) paste(i, features[i])
    ), collapse=', ')

    plot(
        x$feature_id,
        x$center,
        col=x$label,
        pch=paste(x$label),
        main=main,
        xlab=xlab,
        ylab='centers',
        ylim=c(-2, 2)
    )

}

labeled.centers = centers_by_label(labeled, features.selection)
winners.centers = centers_by_label(winners, features.selection)
losers.centers = centers_by_label(losers, features.selection)

plot_centers_by_label(labeled.centers, features.selection, '[Exploring] Centers')
plot_centers_by_label(winners.centers, features.selection, '[Exploring] Centers - winners')
plot_centers_by_label(losers.centers, features.selection, '[Exploring] Centers - losers')

# TODO Champ analisys
# sort(table(winners[, 'championId']), decreasing=TRUE)
# sort(table(winners[winners$label == 3, 'championId']), decreasing=TRUE)
