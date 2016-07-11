options(scipen=999)

library('modules')
import('fun', attach=TRUE)
import('outliers', attach=TRUE)
import('utils', attach=TRUE)

CLOSE_PLOT = TRUE

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

features.logic = c(
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

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features.
outliers = save_plot(function () {
    return(outlier_analysis(data[, features.numeric], factor=3))
}, '../output/outliers-for-each-one', width=16, height=9, close=CLOSE_PLOT)
# outliers$total
#> [1] 10814
# t(outliers$thresholds)
#>                                  lower    upper
#> kills                           -19.00     30.0
#> assists                         -19.00     37.0
#> deaths                          -11.00     24.0
#> goldEarned                    -7465.00  29957.0
#> totalDamageDealt            -229216.25 433109.8
#> magicDamageDealt            -112411.75 169991.0
#> physicalDamageDealt         -260943.50 380499.8
#> totalDamageDealtToChampions  -33290.00  66089.0
#> totalDamageTaken             -25844.25  69949.0
#> minionsKilled                  -347.00    556.0
#> neutralMinionsKilled            -59.00     81.0
#> totalTimeCrowdControlDealt    -1023.00   1567.0
#> wardsPlaced                     -17.00     32.0
#> towerKills                       -3.00      4.0
#> largestMultiKill                 -2.00      5.0
#> largestKillingSpree             -12.00     16.0
#> largestCriticalStrike         -1653.00   2204.0
#> totalHeal                     -7896.00  11865.0

# filter extreme outliers
data = data[!outliers$outliers, ]
# nrow(data)
#> [1] 74656

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these invalid matches need to be removed.
valid_matches = names(Filter(Curry(eq, 10), counter(data$matchId)))
data = data[data$matchId %in% valid_matches, ]
write.csv(data, '../data/treated.csv', row.names=FALSE)
# nrow(data)
#> [1] 35140

# Data normalization (z-score) ------------------------------------------------

# Since the features are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer features using Z-score. Logic features do not need be normalized.
data.normalized = cbind(data[, features.logic], scale(data[, features.numeric]))
write.csv(data.normalized, '../data/normalized.csv', row.names=FALSE)

# Correlation analysis --------------------------------------------------------

# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.
correlations = save_plot(function () {
    return(correlation_analysis(data.normalized)$estimates)
}, '../output/correlation', width=16, height=9, close=CLOSE_PLOT)
write.csv(correlations$estimates, '../data/correlations.csv')

# Rank the hightly correlated features by mean of correlations for each one
features.ranked = names(rev(sort(colMeans(abs(correlations), na.rm=TRUE))))
#> [1] "goldEarned"                  "totalDamageDealt"
#> [3] "totalDamageDealtToChampions" "kills"
#> [5] "physicalDamageDealt"         "largestKillingSpree"
#> [7] "minionsKilled"               "largestMultiKill"
#> [9] "totalDamageTaken"            "towerKills"
#> [11] "largestCriticalStrike"       "neutralMinionsKilled"
#> [13] "assists"                     "totalTimeCrowdControlDealt"
#> [15] "magicDamageDealt"            "wardsPlaced"
#> [17] "totalHeal"                   "deaths"
#> [19] "firstTowerKill"              "firstBloodKill"
#> [21] "firstTowerAssist"

# Dimensionality reduction ----------------------------------------------------

# The features with high affinity (dendogram plot) and high correlation
# (heatmap plot) > 0.7 are redundants. The following are redundant features and
# logic features.
features.unselect = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions',  # (goldErned, totalDamageDealtToChampions)
    'largestMultiKill',  # (Kills, LargestMultiKill)
    'largestKillingSpree',  # (Kills, LargestMultiKill), LargestKillingSpree
    'firstTowerAssist',  # logic
    'firstBloodKill',  # logic
    'firstTowerKill'  # logic
)

# Ranked feature selection.
features.selection = setdiff(features.ranked, features.unselect)
#> [1] "goldEarned"                 "kills"
#> [3] "physicalDamageDealt"        "minionsKilled"
#> [5] "totalDamageTaken"           "towerKills"
#> [7] "largestCriticalStrike"      "neutralMinionsKilled"
#> [9] "assists"                    "totalTimeCrowdControlDealt"
#> [11] "magicDamageDealt"           "wardsPlaced"
#> [13] "totalHeal"                  "deaths"

# Top 3 hightly correled features of the selection
features.top = features.selection[1:3]

# Dimensionality reduction of the normalized data with selected features
data.reduced = data.normalized[, features.selection]

# Learning model (K-means) ----------------------------------------------------

# Perform a cluster analysis on data using k-means for each k = [1:kmax]. Also
# render a knee of the error curve plot to find the optimal k
fits = save_plot(function () {
    return(cluster_analysis(data.reduced, kmax=30)$fits)
}, '../output/k-means-error-curve', close=CLOSE_PLOT)

# Which is the optimal fit in this case? Analysing the error curve plot, the
# k = 7 fit seems to have the best trade-off, as the rate difference does not
# vary so much after it.
fit = fits[[7]]
each(function (i) write.csv(fit[i], strf('../output/fit/%s.csv', i)), names(fit))

# Write or load labeled data --------------------------------------------------

# Associating each reduced data point with its info and label features
labeled = cbind(data[, features.info], label=fit$cluster, data.reduced)
write.csv(labeled, '../data/labeled.csv', row.names=FALSE)

# cluster = read.csv('../output/fit/cluster.csv')$x
# labeled = cbind(data[, features.info], label=cluster, data.reduced)
# labeled = read.csv('../data/labeled.csv')

# Discriminate labeled data between winners and losers
winners = labeled[labeled$winner == 1, ]
losers = labeled[labeled$winner == 0, ]

# Statistical analysis of the results -----------------------------------------

# Hypothesis 1. H1-0: There is no difference between the distributions of the
# clusters found in the learning model; H1-1 There is difference between the
# distributions of the clusters found in the learning model. Test:
# Kruskal-Wallis rank sum test

# Alternative hypothesis true: p.value < 0.05
h1 = kruskal.test(rowSums(labeled[, features.selection]), labeled$label)

# Hypothesis 2. H2-0: For each cluster found in the learning model there is no
# difference between the medians of the winning players and losing players;
# (H2-1) for each cluster found there is difference between the medians of the
# winning players and losing players. Test: Wilcoxon rank sum test with
# continuity correction

# Alternative hypothesis true for each cluster: p.value < 0.05
h2.p.values = values(Map(function (k) {
    x = rowSums(winners[winners$label == k, features.selection])
    y = rowSums(losers[losers$label == k, features.selection])
    wilcox.test(x , y, paired=FALSE)$p.value
}, range(fit$k)))

save_plot(function () {
    par(mfrow=c(1, 2))
    plot(1, h1$p.value, main='Hypothesis - H1', xlab='h1', ylab='p.value')
    plot(h2.p.values, main='Hypothesis - H2', xlab='k', ylab='p.values')
}, '../output/hypothesis', width=16, height=9, close=CLOSE_PLOT)

# Exploring labeled data ------------------------------------------------------

# Plot of labeled data. Only the top selected features
save_plot(function () {
    main = 'Exploring - Scatter plot'
    plot(labeled[, features.top], main=main, col=labeled$label)
}, '../output/exploring-scatter-plot', close=CLOSE_PLOT)

# Only winners
save_plot(function () {
    main = 'Exploring - Scatter plot winners'
    plot(winners[, features.top], main=main, col=winners$label)
}, '../output/exploring-scatter-plot-winners', close=CLOSE_PLOT)

# Only losers
save_plot(function () {
    main = 'Exploring - Scatter plot losers'
    plot(losers[, features.top], main=main, col=losers$label)
}, '../output/exploring-scatter-plot-losers', close=CLOSE_PLOT)

# 3-D visualization of 3 principal components of the labeled data
save_plot(function () {
    par(mfrow=c(1, 3))
    lim = c(-10, 10)
    angle = 95
    pca_plot(labeled[, features.selection], main='PCA', color=labeled$label,
             angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(winners[, features.selection], main='PCA winners',
             color=winners$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(losers[, features.selection], main='PCA losers',
             color=losers$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
}, '../output/exploring-pca', width=18, height=9, close=CLOSE_PLOT)

# In general, we can observe the k clusters found in k-means clustering. We can
# also observe that some clusters are more perceptible than others when the
# labeled data is discriminated between winners and losers.

# Centroid analysis -----------------------------------------------------------
# Given a data set x, summarize the mean for each feature by label.
save_plot(function () {
    par(mfrow=c(3,1))
    lim = c(-2, 2)
    plot_by(labeled[, features.selection], labeled$label, mean, ylim=lim)
    plot_by(winners[, features.selection], winners$label, mean, ylim=lim)
    plot_by(losers[, features.selection], losers$label, mean, ylim=lim)
}, '../output/exploring-centers', width=16, height=9, close=CLOSE_PLOT)
