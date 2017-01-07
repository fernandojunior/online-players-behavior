options(scipen=999)

library('modules')
import('fun', attach=TRUE)
import('outliers', attach=TRUE)
import('utils', attach=TRUE)

RENDER_PLOT_SAVE = TRUE
RENDER_PLOT_CLOSE = FALSE

# Load data -------------------------------------------------------------------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
# data = read.csv('../data/data.csv')
data = read.csv('../data/data20170105025503.csv')

# nrow(data)
# [1] 85470

# Remove duplicated rows by summonerId to decrease bias
data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)
# [1] 54681

features = names(data)

features.info = c(
    'matchId',
    'matchMode',
    'queueType',
    'season',
    'championId',
    'summonerId',
    'matchDuration',
    'matchCreation',
    'matchCreationYear',
    'matchCreationMonth',
    'matchCreationDay',
    'matchCreationHour',
    'item0',
    'item1',
    'item2',
    'item3',
    'item4',
    'item5',
    'item6',
    'combatPlayerScore',
    'objectivePlayerScore',
    'totalPlayerScore',
    'totalScoreRank',
    'unrealKills'  # ARAM
)

features.target = 'winner'

features.logical = c(
    # 'firstBloodAssist',
    # 'firstBloodKill',
    'firstInhibitorAssist',
    'firstInhibitorKill'
    # 'firstTowerAssist',
    # 'firstTowerKill'
)

features.categorical = c('champLevel')

features.numeric = setdiff(features, c(
    features.target,
    features.info,
    features.logical,
    features.categorical
))
# [1] "assists"                         "deaths"
# [3] "doubleKills"                     "goldEarned"
# [5] "goldSpent"                       "inhibitorKills"
# [7] "killingSprees"                   "kills"
# [9] "largestCriticalStrike"           "largestKillingSpree"
# [11] "largestMultiKill"                "magicDamageDealt"
# [13] "magicDamageDealtToChampions"     "magicDamageTaken"
# [15] "minionsKilled"                   "neutralMinionsKilled"
# [17] "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [19] "pentaKills"                      "physicalDamageDealt"
# [21] "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [23] "quadraKills"                     "sightWardsBoughtInGame"
# [25] "totalDamageDealt"                "totalDamageDealtToChampions"
# [27] "totalDamageTaken"                "totalHeal"
# [29] "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [31] "towerKills"                      "tripleKills"
# [33] "trueDamageDealt"                 "trueDamageDealtToChampions"
# [35] "trueDamageTaken"                 "visionWardsBoughtInGame"
# [37] "wardsKilled"                     "wardsPlaced"

features.ong = c(
    'firstBloodKill', 'firstTowerKill', 'firstTowerAssist',
    'kills', 'assists', 'deaths', 'goldEarned','totalDamageDealt',
    'magicDamageDealt', 'physicalDamageDealt', 'totalDamageDealtToChampions',
    'totalDamageTaken', 'minionsKilled', 'neutralMinionsKilled',
    'totalTimeCrowdControlDealt', 'wardsPlaced', 'towerKills',
    'largestMultiKill', 'largestKillingSpree', 'largestCriticalStrike',
    'totalHeal'
)

features.ong.numeric = c(
    'goldEarned', 'kills', 'physicalDamageDealt', 'minionsKilled',
    'totalDamageTaken', 'towerKills', 'largestCriticalStrike',
    'neutralMinionsKilled', 'assists', 'totalTimeCrowdControlDealt',
    'magicDamageDealt', 'wardsPlaced', 'totalHeal', 'deaths'
)

# features with low variance
features.low_variance = filter_features(data[, features.numeric], var, max=8)
#  [1] "doubleKills"             "inhibitorKills"
#  [3] "killingSprees"           "largestKillingSpree"
#  [5] "largestMultiKill"        "pentaKills"
#  [7] "quadraKills"             "sightWardsBoughtInGame"
#  [9] "towerKills"              "tripleKills"
# [11] "visionWardsBoughtInGame" "wardsKilled"

# Pre-select features with relevant variance
features.selection = setdiff(features.numeric, features.low_variance)
# [1] "assists"                         "deaths"
# [3] "goldEarned"                      "goldSpent"
# [5] "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealt"                "magicDamageDealtToChampions"
# [9] "magicDamageTaken"                "minionsKilled"
# [11] "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle"
# [13] "neutralMinionsKilledTeamJungle"  "physicalDamageDealt"
# [15] "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [17] "totalDamageDealt"                "totalDamageDealtToChampions"
# [19] "totalDamageTaken"                "totalHeal"
# [21] "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [23] "trueDamageDealt"                 "trueDamageDealtToChampions"
# [25] "trueDamageTaken"                 "wardsPlaced"

# Treatment of outliers -------------------------------------------------------

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features.
outliers = render_plot(function () {
    return(outlier_analysis(data[, features.selection], factor=3))
}, '../output/outliers-for-each-one', width=16, height=12)
# outliers$total
#> [1] 11086
# > t(outliers$thresholds)
#                                   lower  upper
# assists                             -19     37
# deaths                              -11     24
# goldEarned                        -7491  30008
# goldSpent                         -7420  27895
# kills                               -19     30
# largestCriticalStrike             -1644   2192
# magicDamageDealt                -110875 168117
# magicDamageDealtToChampions      -26558  39151
# magicDamageTaken                 -15235  30482
# minionsKilled                      -351    559
# neutralMinionsKilled                -59     81
# neutralMinionsKilledEnemyJungle      -9     12
# neutralMinionsKilledTeamJungle      -48     64
# physicalDamageDealt             -259807 378810
# physicalDamageDealtToChampions   -34075  49036
# physicalDamageTaken              -18829  45690
# totalDamageDealt                -232569 435658
# totalDamageDealtToChampions      -33410  66123
# totalDamageTaken                 -26110  70511
# totalHeal                         -7933  11940
# totalTimeCrowdControlDealt        -1026   1571
# trueDamageDealt                  -15569  21020
# trueDamageDealtToChampions        -3132   4176
# trueDamageTaken                   -2193   3554
# wardsPlaced                         -17     32

# Select only features where lower != upper from outlier analysis
features.selection = colnames(outliers$thresholds)
# [1] "assists"                         "deaths"
# [3] "goldEarned"                      "goldSpent"
# [5] "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealt"                "magicDamageDealtToChampions"
# [9] "magicDamageTaken"                "minionsKilled"
# [11] "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle"
# [13] "neutralMinionsKilledTeamJungle"  "physicalDamageDealt"
# [15] "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [17] "totalDamageDealt"                "totalDamageDealtToChampions"
# [19] "totalDamageTaken"                "totalHeal"
# [21] "totalTimeCrowdControlDealt"      "trueDamageDealt"
# [23] "trueDamageDealtToChampions"      "trueDamageTaken"
# [25] "wardsPlaced"

# remove extreme outliers
data = data[!outliers$outliers, ]
# nrow(data)
#> [1] 43595

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these invalid (incomplete) matches need to be removed.
valid_matches = names(Filter(Curry(eq, 10), counter(data$matchId)))
data = data[data$matchId %in% valid_matches, ]
# write.csv(data, '../data/treated.csv', row.names=FALSE)
# nrow(data)
#> [1] 2400

# Data normalization ----------------------------------------------------------

# As the match duration varies between the matches, the features were divided
# by match duration to compute players performance per minute.
data.performance = data[, features.numeric]/data[, 'matchDuration']

# team performance per minute
team.performance = aggregate(. ~ matchId + winner, data=cbind(data[, c('matchId', 'winner')], data.performance),  FUN=sum)
team.performance.logical = aggregate(. ~ matchId + winner, data=cbind(data[, c('matchId', 'winner', features.logical)]),  FUN=max)

# What is the player performance in relation to the team?
# As the player performance varies between the matches and the features also
# are of different varieties, the player performance was divided by team
# performance to compute relative performance, wich can ranges from 0 to 1, and
# thus scale up the data in a uniform way. Relative performance of the players
# in their respective teams:
data.relative_performance = as.data.frame(t(map(function (i) {
    x = cbind(data[i, c('matchId', 'winner')], data.performance[i, ])
    t = team.performance[team.performance$matchId == x$matchId & team.performance$winner == x$winner, ]
    x[, features.numeric]/t[, features.numeric]
}, 1:nrow(data.performance))))

data.relative_performance = sapply(data.relative_performance, as.numeric)
data.relative_performance[is.nan(data.relative_performance)] <- 0

team.performance = sapply(team.performance, as.numeric)
team.performance.logical = sapply(team.performance.logical, as.numeric)
team.performance[is.nan(team.performance)] <- 0

# Boolean features do not need be normalized
data.normalized = na.omit(cbind(
    data[, c('matchId', 'winner')],
    data[, features.logical],
    data.relative_performance[, features.numeric]
))

# Since the team performances are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the features.
team = as.data.frame(na.omit(cbind(
    team.performance[, c('matchId', 'winner')],
    team.performance.logical[, features.logical],
    team.performance[, features.numeric]
)))

team.normalized = as.data.frame(na.omit(cbind(
    team.performance[, c('matchId', 'winner')],
    team.performance.logical[, features.logical],
    normalize(team.performance[, features.numeric])
)))

# Correlation analysis --------------------------------------------------------

# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.
correlations = render_plot(function () {
    return(correlation_analysis(data.normalized[, features.selection])$estimates)
}, '../output/correlation-player', width=18, height=12)

correlations = render_plot(function () {
    return(correlation_analysis(team.normalized[, features.selection])$estimates)
}, '../output/correlation-team', width=18, height=12)

# Rank the most correlated features by mean of correlations for each one
features.selection = names(rev(sort(colMeans(abs(correlations), na.rm=TRUE))))
# [1] "goldEarned"                      "goldSpent"
# [3] "totalDamageDealt"                "neutralMinionsKilledEnemyJungle"
# [5] "kills"                           "totalDamageDealtToChampions"
# [7] "assists"                         "physicalDamageDealt"
# [9] "minionsKilled"                   "deaths"
# [11] "physicalDamageDealtToChampions"  "neutralMinionsKilled"
# [13] "magicDamageDealtToChampions"     "magicDamageDealt"
# [15] "largestCriticalStrike"           "totalTimeCrowdControlDealt"
# [17] "totalHeal"                       "wardsPlaced"
# [19] "trueDamageDealt"                 "totalDamageTaken"
# [21] "neutralMinionsKilledTeamJungle"  "physicalDamageTaken"
# [23] "magicDamageTaken"                "trueDamageDealtToChampions"
# [25] "trueDamageTaken"

# Dimensionality reduction ----------------------------------------------------

# Features with high similarity (dendogram plot) and high correlation (heatmap
# plot) > 0.7 are redundants.
features.redundant = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions', # physicalDamageDealtToChampions + magicDamageDealtToChampions
    'totalDamageTaken', # physicalDamageTaken + magicDamageTaken
    'neutralMinionsKilled', # neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle
    'physicalDamageDealt', # physicalDamageDealt x physicalDamageDealtToChampions
    'magicDamageDealt', # magicDamageDealt x magicDamageDealtToChampions
    # 'trueDamageDealt', # trueDamageDealt x trueDamageDealtToChampions
    'goldEarned', # goldEarned x goldSpent
    'goldSpent' # goldSpent x kills
)

features.redundant.team = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions', # physicalDamageDealtToChampions + magicDamageDealtToChampions
    'totalDamageTaken', # physicalDamageTaken + magicDamageTaken
    'neutralMinionsKilled', # neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle
    'physicalDamageDealt', # physicalDamageDealt x physicalDamageDealtToChampions
    'magicDamageDealt', # magicDamageDealt x magicDamageDealtToChampions
    'goldEarned', # goldEarned x goldSpent
    'goldSpent' # goldSpent x kills
    #'assists' # assists x kills
)

# Remove redundant features (high similarity and correlation) to avoid
# multicollinearity.
features.selection.player = setdiff(features.selection, features.redundant)
# [1] "neutralMinionsKilledEnemyJungle" "kills"
# [3] "assists"                         "minionsKilled"
# [5] "deaths"                          "physicalDamageDealtToChampions"
# [7] "magicDamageDealtToChampions"     "largestCriticalStrike"
# [9] "totalTimeCrowdControlDealt"      "totalHeal"
# [11] "wardsPlaced"                     "neutralMinionsKilledTeamJungle"
# [13] "physicalDamageTaken"             "magicDamageTaken"
# [15] "trueDamageDealtToChampions"      "trueDamageTaken"

features.selection.team = setdiff(features.selection, features.redundant.team)
# [1] "neutralMinionsKilledEnemyJungle" "kills"
# [3] "minionsKilled"                   "deaths"
# [5] "physicalDamageDealtToChampions"  "magicDamageDealtToChampions"
# [7] "largestCriticalStrike"           "totalTimeCrowdControlDealt"
# [9] "totalHeal"                       "wardsPlaced"
# [11] "trueDamageDealt"                 "neutralMinionsKilledTeamJungle"
# [13] "physicalDamageTaken"             "magicDamageTaken"
# [15] "trueDamageDealtToChampions"      "trueDamageTaken"

# Dimensionality reduction of the normalized data with selected features
data.reduced = data.normalized[, features.selection.player]

team.reduced = team.normalized[, features.selection.team]

correlations = render_plot(function () {
    return(correlation_analysis(data.reduced)$estimates)
}, '../output/correlation-player-thin', width=18, height=12)

correlations = render_plot(function () {
    return(correlation_analysis(team.reduced)$estimates)
}, '../output/correlation-team-thin', width=18, height=12)

# Cluster analysis (K-means) --------------------------------------------------

# Perform a cluster analysis on data using k-means for each k = [1:kmax]. Also
# render a knee of the error curve plot to find the optimal k
fits = render_plot(function () {
    return(cluster_analysis(data.reduced, kmax=120)$fits)
}, '../output/k-means-error-curve-player')

render_plot(function () {
    return(bagging_cluster_analysis(data.reduced, kmax=120, ntests=10))
}, '../output/k-means-error-curve-player-avaraged')

render_plot(function () {
    return(bagging_cluster_analysis(data.normalized, kmax=120, ncol=ncol(data.reduced), ntests=10))
}, '../output/k-means-error-curve-player-bagging')

fits.team = render_plot(function () {
    return(cluster_analysis(team.reduced, kmax=120)$fits)
}, '../output/k-means-error-curve-team')

render_plot(function () {
    return(bagging_cluster_analysis(team.reduced, kmax=120, ntests=10))
}, '../output/k-means-error-curve-team-avareged')

render_plot(function () {
    return(bagging_cluster_analysis(team.normalized, kmax=120, ncol=ncol(team.reduced), ntests=10))
}, '../output/k-means-error-curve-team-bagging')

# Which is the optimal fit in this case? Analysing the error curve plot, the
# k = 7 fit seems to have the best trade-off, as the rate difference does not
# vary so much after it.
fit = fits[[10]]
# each(function (i) write.csv(fit[i], strf('../output/fit/%s.csv', i)), names(fit))

fit.team = fits.team[[7]]

# TODO Write or load labeled data ---------------------------------------------

# Associating each reduced data point with its info and label features
labeled = cbind(winner=data[, 'winner'], data[, features.info], label=fit$cluster, data.reduced)
labeled.team = cbind(team.normalized[, c('matchId', 'winner')], label=fit.team$cluster, team.reduced)
# write.csv(labeled, '../data/labeled.csv', row.names=FALSE)

# cluster = read.csv('../output/fit/cluster.csv')$x
# labeled = cbind(data[, features.info], label=cluster, data.reduced)
# labeled = read.csv('../data/labeled.csv')

# Balancing/undersampling data ................................................

# Discriminate labeled data between winners and losers
winners = labeled[labeled$winner == 1, ]
losers = labeled[labeled$winner == 0, ]

winners.team = labeled.team[labeled.team[, 'winner'] == 1, ]
losers.team = labeled.team[labeled.team[, 'winner'] == 0, ]

# Clusters size analysis ......................................................

clusters_size = as.data.frame(cbind(
    all=table(labeled[, 'label']),
    winners=table(winners[, 'label']),
    losers=table(losers[, 'label'])
))

clusters_size.team = as.data.frame(cbind(
    all=table(labeled.team[, 'label']),
    winners=table(winners.team[, 'label']),
    losers=table(losers.team[, 'label'])
))

# relative clusters size between winners and losers
clusters_size.relative = as.data.frame(cbind(
    winners=clusters_size$winners / clusters_size$all,
    losers=clusters_size$losers / clusters_size$all
))

clusters_size.team.relative = as.data.frame(cbind(
    winners=clusters_size.team$winners / clusters_size.team$all,
    losers=clusters_size.team$losers / clusters_size.team$all
))

# Check if there are outliers in clusters size
boxplot(values(clusters_size.relative))$stats

boxplot(values(clusters_size.team.relative))$stats

# Balancing team data ....................................................

# min clusters size between winners and losers
clusters_size.min = min(table(winners$label), table(losers$label))

clusters_size.team.min = min(table(winners.team[, 'label']), table(losers.team[, 'label']))

# undersampling
winners = undersample(winners, 'label', clusters_size.min)
losers = undersample(losers, 'label', clusters_size.min)
labeled = rbind(winners, losers)

winners.team = undersample(winners.team, 'label', clusters_size.team.min)
losers.team = undersample(losers.team, 'label', clusters_size.team.min)
labeled.team = rbind(winners.team, losers.team)

# TODO Statistical analysis of the results ------------------------------------

# Hypothesis 1. H1-0: There is no difference between the distributions of the
# clusters found in the learning model; H1-1 There is difference between the
# distributions of the clusters found in the learning model. Test:
# Kruskal-Wallis rank sum test

# Alternative hypothesis true: p.value < 0.05
h1 = kruskal.test(rowSums(labeled[, features.selection.player]), labeled$label)

# Hypothesis 2. H2-0: For each cluster found in the learning model there is no
# difference between the medians of the winning players and losing players;
# (H2-1) for each cluster found there is difference between the medians of the
# winning players and losing players. Test: Wilcoxon rank sum test with
# continuity correction

# Alternative hypothesis true for each cluster: p.value < 0.05
h2.p.values = values(Map(function (k) {
    x = rowSums(winners[winners$label == k, features.selection.player])
    y = rowSums(losers[losers$label == k, features.selection.player])
    # TODO print(paste(shapiro.test(x)$p.value, shapiro.test(y)$p.value))
    # TODO print(t.test(x, y, paired=FALSE)$p.value)
    wilcox.test(x , y, paired=FALSE)$p.value
}, range(fit$k)))

render_plot(function () {
    par(mfrow=c(1, 2))
    plot(1, h1$p.value, main='Hypothesis - H1', xlab='h1', ylab='p.value')
    plot(h2.p.values, main='Hypothesis - H2', xlab='k', ylab='p.values')
}, '../output/hypothesis-player', width=16, height=9)

# TODO compare median between clusters

# Exploring labeled data ------------------------------------------------------

# Plot of labeled data. Only the top selected features
render_plot(function () {
    main = 'Exploring - Scatter plot'
    plot(labeled[, features.selection.player[1:3]], main=main, col=labeled$label)
}, '../output/exploring-scatter-plot')

# Only winners
render_plot(function () {
    main = 'Exploring - Scatter plot winners'
    plot(winners[, features.selection.player[1:3]], main=main, col=winners$label)
}, '../output/exploring-scatter-plot-winners')

# Only losers
render_plot(function () {
    main = 'Exploring - Scatter plot losers'
    plot(losers[, features.selection.player[1:3]], main=main, col=losers$label)
}, '../output/exploring-scatter-plot-losers')

# 3-D visualization of 3 principal components of the labeled data
render_plot(function () {
    par(mfrow=c(1, 3))
    lim = c(-2, 2)
    angle = 95
    pca_plot(labeled[, features.selection.player], main='PCA', color=labeled$label,
             angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(winners[, features.selection.player], main='PCA winners',
             color=winners$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(losers[, features.selection.player], main='PCA losers',
             color=losers$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
}, '../output/exploring-pca', width=18, height=9)

# In general, we can observe the k clusters found in k-means clustering. We can
# also observe that some clusters are more perceptible than others when the
# labeled data is discriminated between winners and losers.

# TODO Centroid analysis ------------------------------------------------------
# Given a data set x, summarize the mean for each feature by label.
render_plot(function () {
    par(mfrow=c(3,1))
    lim = c(-1, 1)
    plot_by(labeled[, features.selection.player], labeled$label, mean, ylim=lim)
    plot_by(winners[, features.selection.player], winners$label, mean, ylim=lim)
    plot_by(losers[, features.selection.player], losers$label, mean, ylim=lim)
}, '../output/exploring-centers-player', width=16, height=9)

render_plot(function () {
    par(mfrow=c(3,1))
    lim = c(-0.1, 0.1)
    plot_by(labeled.team[, features.selection.team], labeled.team$label, mean, ylim=lim)
    plot_by(winners.team[, features.selection.team], winners.team$label, mean, ylim=lim)
    plot_by(losers.team[, features.selection.team], losers.team$label, mean, ylim=lim)
}, '../output/exploring-centers-team', width=16, height=9)
