source('utils.R')

options(scipen=999)

# Load data -------------------------------------------------------------------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('../data/data.csv')
# nrow(data)
# [1] 85470

features = names(data)

features.info = c(
    'matchId',
    'matchCreation',
    'summonerId',
    'championId',
    'winner'
)

features.boolean = c(
    'firstBloodKill',
    'firstTowerKill',
    'firstTowerAssist'
)

features.numeric = c(
    'kills',
    'assists',
    'deaths',
    'goldEarned',
    'totalDamageDealt',
    'magicDamageDealt',
    'physicalDamageDealt',
    'totalDamageDealtToChampions',
    'totalDamageTaken',
    'minionsKilled',
    'neutralMinionsKilled',
    'totalTimeCrowdControlDealt',
    'wardsPlaced',
    'towerKills',
    'largestMultiKill',
    'largestKillingSpree',
    'largestCriticalStrike',
    'totalHeal'
)

# Treatment of outliers -------------------------------------------------------

# Boxplot to analyze the outliers of all numeric features.
save.boxplot(
    data[, features.numeric],
    main='Outliers - All numeric features 1',
    names=range(length(features.numeric))
)

# As we can see from the above plot that some features has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
x11(width=16, height=9)
par(mfrow=c(3, 6))
for (feature in features.numeric)
    boxplot(data[, feature], main=strf('Outliers - %s', feature))
save.plot.png('Outliers - All numeric features 2', path=PLOT_DIR)

# auto indentify extreme (IQR factor = 3) outliers.
outliers = find_outliers(data[, features.numeric], factor=3)
# t(outliers$total)
#                                  lower    upper
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

# outliers$total
# [1] 10814

# Filtering entire data to remove extreme outliers
data = data[!outliers$outliers, ]
# nrow(data)
# [1] 74656

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent matches need to be removed.

# Number of participants by match ID
participants_by_match = counter(data$matchId)

# Matches (IDs) that do not contain all 10 participants
inconsistent_matches = names(participants_by_match[participants_by_match < 10])

# Removing inconsistent matches from data
data = data[!(data$matchId %in% inconsistent_matches), ]
# nrow(data)
# [1] 35140

write.csv(data, "../data/treated.csv", row.names=FALSE)

# Data normalization (z-score) ------------------------------------------------

# Since the features are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer features using Z-score. Boolean features do not need be normalized.
data.normalized = cbind(
    data[, features.boolean],
    scale(data[, features.numeric])
)

write.csv(data.normalized, "../data/normalized.csv", row.names=FALSE)

# Correlation analysis --------------------------------------------------------
# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.

x11(width=16, height=9)
correlations = correlation_analysis(data.normalized)
save.plot.png('Correlation - Dendrogram and heatmap', path=PLOT_DIR)

write.csv(correlations$estimates, "../data/correlations.csv")

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

# Dimensionality reduction ----------------------------------------------------

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

# Dimensionality reduction of the normalized data with selected features
data.reduced = data.normalized[, features.selection]

# Learning model (K-means) ----------------------------------------------------
# Perform a cluster analysis on reduced data using k-means

# Analyse the knee of the error curve to find the optimal k
x11(width=9, height=9)
fits = cluster_analysis(data.reduced, kmax=30, main='K-means - Error curve')$fits
save.plot.png('K-means - Error curve', path=PLOT_DIR)

# Which is the optimal fit in this case for k = {5, ..., 9}? Let's analyse the
# between-cluster SSE rate (betweenss / totss) difference for each one.

# Between-cluster SSE rate differences for each k = {5, ..., 9} k-means fit
bssrd = map(
    function(k) betweenss.rate(fits[k, ]) - betweenss.rate(fits[k - 1, ]),
    range(5, 9)
)

# Plot to visualuze the between-cluster SSE rate differences
save.plot(
    range(5, 9),
    bssrd,
    main='K-means - Between-cluster SSE rate differences',
    xlab='k fit'
)

# Analysing the between-cluster SSE rate differences, the k = 7 fit seems to
# have the best trade-off, as the rate difference does not vary so much after
# it. Let's add some extra components and save it.
fit = fits[7, ]

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

# Load previous labeled data
# cluster = read.csv('../output/fit/cluster.csv')$x
# labeled = cbind(data[, features.info], label=cluster, data.reduced)

# Spliting labeled data between winners and losers
winners = labeled[labeled$winner == 1, ]
losers = labeled[labeled$winner == 0, ]

# Statistical analysis of the results -----------------------------------------

# Hypothesis 1. H1-0: There is no difference between the distributions of the
# clusters found in the learning model; H1-1 There is difference between the
# distributions of the clusters found in the learning model. Test:
# Kruskal-Wallis rank sum test

h1 = kruskal.test(rowSums(labeled[, features.selection]), labeled$label)
# Alternative hypothesis true: p.value < 0.05
save.plot(1, h1$p.value, main='Hypothesis - H1', xlab='h1', ylab='p.value')

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
save.plot(h2.p.values, main='Hypothesis - H2', xlab='k', ylab='p.values')

# Exploring labeled data ------------------------------------------------------

# A sample with n = 80 random points from labeled data
data.sampled = labeled[sample(range(nrow(labeled)), 80), ]

# Clusplot of sampled data
save.clusplot(
    data.sampled[, features.selection],
    data.sampled$label,
    main='Exploring - Cluster plot of the labeled data n=80',
    labels=4,
    col.clus= sort(unique(data.sampled$label)),
    col.p=data.sampled$label,
    lines=0
)

# Plot of labeled data. Only the top selected features
save.plot(
    labeled[, features.topselection],
    main='Exploring - Scatter plot',
    col=labeled$label
)

# Only winners
save.plot(
    winners[, features.topselection],
    main='Exploring - Scatter plot winners',
    col=winners$label
)

# Only losers
save.plot(
    losers[, features.topselection],
    main='Exploring - Scatter plot losers',
    col=losers$label
)

# PCA of labeled data
labeled.pca = prcomp(labeled[, features.selection], center=TRUE)
winners.pca = prcomp(winners[, features.selection], center=TRUE)
losers.pca = prcomp(losers[, features.selection], center=TRUE)

# Selecting principal components to view
pca_indices = range(3)

# 3-D visualization of principal components of the labeled data
x11(width=18, height=9)
par(mfrow=c(1, 3))
if (!require('scatterplot3d'))
    library('scatterplot3d')
scatterplot3d(
    labeled.pca$x[, pca_indices],
    main='Exploring - PCA',
    color=labeled$label,
    angle=95,
    xlim=c(-10, 10),
    ylim=c(-10, 10),
    zlim=c(-10, 10)
)
scatterplot3d(
    winners.pca$x[, pca_indices],
    main='Exploring - PCA winners',
    color=winners$label,
    angle=95,
    xlim=c(-10, 10),
    ylim=c(-10, 10),
    zlim=c(-10, 10)
)
# 3-D visualization of principal components of losers
scatterplot3d(
    losers.pca$x[, pca_indices],
    main='Exploring - PCA losers',
    color=losers$label,
    angle=95,
    xlim=c(-10, 10),
    ylim=c(-10, 10),
    zlim=c(-10, 10)
)
save.plot.png('Exploring - PCA', path=PLOT_DIR)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is discriminated between winners and losers.

# Centroid analysis
# -----------------
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

    plot(x$feature_id, x$center, col=x$label, pch=paste(x$label), main=main,
         xlab=xlab, ylab='centers', ylim=c(-2, 2)
    )

}

labeled.centers = centers_by_label(labeled, features.selection)
winners.centers = centers_by_label(winners, features.selection)
losers.centers = centers_by_label(losers, features.selection)

x11(width=16, height=9)
par(mfrow=c(3,1))
plot_centers_by_label(labeled.centers, features.selection, 'Exploring - Centers')
plot_centers_by_label(winners.centers, features.selection, 'Exploring - Centers winners')
plot_centers_by_label(losers.centers, features.selection, 'Exploring - Centers losers')
save.plot.png('Exploring - Centers', path=PLOT_DIR)

# TODO function to save plot using callback

# TODO function for outliers analysis

# TODO As the match duration varies between the matches, the features must be divided by match duration.

# TODO remove duplicated summonerIds and choose only a participant for each match

# TODO win rate analysis
# (counter(winners$label) - counter(losers$label)) / (counter(winners$label) + counter(losers$label))

# TODO Champ analisys
# sort(table(winners[, 'championId']), decreasing=TRUE)
# sort(table(winners[winners$label == 3, 'championId']), decreasing=TRUE)
