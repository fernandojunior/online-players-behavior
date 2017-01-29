options(scipen=999)
options("width"=200)

library('modules')
import('fun', attach=TRUE)
import('outliers', attach=TRUE)
import('feature_selection', attach=TRUE)
import('utils', attach=TRUE)

RENDER_PLOT_SAVE = TRUE
RENDER_PLOT_CLOSE = FALSE

# =========================
# Specific domain functions
# =========================

#' Return matches with 10 players
get_10_players_matches = function (players) {
    matchIds = names(Filter(Curry(eq, 10), table(players$matchId)))
    players = players[players$matchId %in% matchIds, ]
}

#' Return matches with 5 players each team (5x5)
get_5x5_matches = function (players) {
    players = get_10_players_matches(players)
    teams = aggregate(winner~matchId, players[, c('winner', 'matchId')], function(x) sum(as.numeric(x)))
    matchIds = teams[teams$winner == 5, 'matchId']
    return(players[players$matchId %in% matchIds, ])
}

# =========
# Load data
# =========

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
# data = read.csv('../data/data.csv')
data = read.csv('../data/data20170105025503.csv')

# Some games are with error (from Riot API).
# Remove inconsistent matches, i.e. matches not 5x5
data = get_5x5_matches(data)

# nrow(data)
# [1] 1099950

# Remove duplicated rows by summonerId to decrease bias
# data = data[!duplicated(data[, 'summonerId']),]
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

# ======================
# Treatment of outliers
# ======================

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
# 10 participants. So, these inconsistent (incomplete) matches need to be removed.
data = get_5x5_matches(data)
# nrow(data)
#> [1] 207990

# ==================
# Data normalization
# ==================

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
rownames(data.relative_performance) = rownames(data.performance)

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

# ====================
# Correlation analysis
# ====================

# Correlation matrix of normalized data using Spearman method, which does not
# require the features follow a normal distribuition or linear correlation.
correlations = render_plot(function () {
    return(correlation_analysis(data.normalized[, features.selection])$estimates)
}, '../output/correlation-player', width=18, height=12)

correlations = render_plot(function () {
    return(correlation_analysis(team.normalized[, features.selection])$estimates)
}, '../output/correlation-team', width=18, height=12)

# ====================================
# Redundant feature selection analysis
# ====================================

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

# Compound features
features.compound = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions', # physicalDamageDealtToChampions + magicDamageDealtToChampions
    'totalDamageTaken', # physicalDamageTaken + magicDamageTaken
    'neutralMinionsKilled' # neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle
)

# Remove compound feaures (leaving only atomic attributes - first normal form)
features.selection = setdiff(features.selection, features.compound)

# Redundant features
features.redundant.player = redundant_features(data.normalized[, features.selection])
# [1] "goldSpent"                   "magicDamageDealtToChampions" "physicalDamageDealt"

features.redundant.team = redundant_features(team.normalized[, features.selection])
# [1] "goldSpent"                   "goldEarned"                  "kills"                       "physicalDamageDealt"         "magicDamageDealtToChampions" "assists"
# [7] "magicDamageTaken

# Remove redundant features: highly similarity (dendogram) and correlation (heatmap).
features.selection.player = setdiff(features.selection, features.redundant.player)
# [1] "assists"                         "goldEarned"                      "kills"                           "physicalDamageDealtToChampions"  "minionsKilled"
# [6] "magicDamageDealt"                "totalHeal"                       "wardsPlaced"                     "physicalDamageTaken"             "neutralMinionsKilledEnemyJungle"
# [11] "trueDamageDealt"                 "trueDamageDealtToChampions"      "wardsKilled"                     "magicDamageTaken"                "totalTimeCrowdControlDealt"
# [16] "totalUnitsHealed"                "trueDamageTaken"                 "deaths"                          "largestCriticalStrike"           "neutralMinionsKilledTeamJungle

features.selection.team = setdiff(features.selection, features.redundant.team)
# [1] "physicalDamageDealtToChampions"  "minionsKilled"                   "magicDamageDealt"                "totalHeal"                       "wardsPlaced"
# [6] "physicalDamageTaken"             "neutralMinionsKilledEnemyJungle" "trueDamageDealt"                 "trueDamageDealtToChampions"      "wardsKilled"
# [11] "totalTimeCrowdControlDealt"      "totalUnitsHealed"                "trueDamageTaken"                 "deaths"                          "largestCriticalStrike"
# [16] "neutralMinionsKilledTeamJungle"

# ========================
# Dimensionality reduction
# ========================

# Dimensionality reduction of the normalized data with selected features
data.reduced = data.normalized[, features.selection.player]

team.reduced = team.normalized[, features.selection.team]

correlations = render_plot(function () {
    return(correlation_analysis(data.reduced)$estimates)
}, '../output/correlation-player-thin', width=18, height=12)

correlations = render_plot(function () {
    return(correlation_analysis(team.reduced)$estimates)
}, '../output/correlation-team-thin', width=18, height=12)

# ==========================
# Cluster analysis (K-means)
# ==========================

# Perform a cluster analysis on data using k-means for each k = [1:kmax]. Also
# render a knee of the error curve plot to find the optimal k
fits = render_plot(function () {
    return(cluster_analysis(data.reduced, kmax=120)$fits)
}, '../output/k-means-error-curve-player')

fits.team = render_plot(function () {
    return(cluster_analysis(team.reduced, kmax=120)$fits)
}, '../output/k-means-error-curve-team')

# render_plot(function () {
#     return(bagging_cluster_analysis(data.reduced, kmax=120, ntests=10))
# }, '../output/k-means-error-curve-player-avaraged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(data.normalized, kmax=120, ncol=ncol(data.reduced), ntests=10))
# }, '../output/k-means-error-curve-player-bagging')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.reduced, kmax=120, ntests=10))
# }, '../output/k-means-error-curve-team-avareged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.normalized, kmax=120, ncol=ncol(team.reduced), ntests=10))
# }, '../output/k-means-error-curve-team-bagging')

# Which is the optimal fit in this case? Analysing the error curve plot, the
# k = 7 fit seems to have the best trade-off, as the rate difference does not
# vary so much after it.
fit = fits[[8]]
# each(function (i) write.csv(fit[i], strf('../output/fit/%s.csv', i)), names(fit))

fit.team = fits.team[[6]]

# Associating each reduced data point with its info and label features
labeled = cbind(winner=data[, 'winner'], data[, features.info], label=fit$cluster, data.reduced)
labeled.team = cbind(team.normalized[, c('matchId', 'winner')], label=fit.team$cluster, team.reduced)

# Labeling data
data = cbind(data, label=fit$cluster)
player = cbind(data.performance, label=fit$cluster)
team = cbind(team, label=fit.team$cluster)

###############################################################################
# Clustered data balancing (undersampling) by discriminating winners and losers
###############################################################################

# Discriminate clustered data between winners and losers
winners = labeled[labeled$winner == 1, ]
losers = labeled[labeled$winner == 0, ]

winners.team = labeled.team[labeled.team[, 'winner'] == 1, ]
losers.team = labeled.team[labeled.team[, 'winner'] == 0, ]

# Clusters size analysis
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

# Relative clusters size between winners and losers
clusters_size.relative = as.data.frame(cbind(
    winners=clusters_size$winners / clusters_size$all,
    losers=clusters_size$losers / clusters_size$all
))

clusters_size.team.relative = as.data.frame(cbind(
    winners=clusters_size.team$winners / clusters_size.team$all,
    losers=clusters_size.team$losers / clusters_size.team$all
))

# Check if there are outliers in relative clusters size
boxplot(values(clusters_size.relative))$stats

boxplot(values(clusters_size.team.relative))$stats

# Min cluster size in winners + losers
clusters_size.min = min(table(winners$label), table(losers$label))

clusters_size.team.min = min(table(winners.team$label), table(losers.team$label))

# Undersampling clustered data based on min size
winners = undersample(winners, 'label', clusters_size.min)
losers = undersample(losers, 'label', clusters_size.min)
labeled = rbind(winners, losers)

winners.team = undersample(winners.team, 'label', clusters_size.team.min)
losers.team = undersample(losers.team, 'label', clusters_size.team.min)
labeled.team = rbind(winners.team, losers.team)

# Re-do correlation and redundant feature analysis of balanced data
correlation_analysis(labeled[, features.selection.player])$estimates >= 0.7
correlation_analysis(labeled.team[, features.selection.team])$estimates >= 0.7

redundant_features(labeled[, features.selection.player])
# NULL
redundant_features(labeled.team[, features.selection.team])
# NULL

##########################################
# TODO Statistical analysis of the results
##########################################

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

########################
# Exploring labeled data
########################

# Plot of labeled data. Only the top selected features
render_plot(function () {
    main = 'Exploring - Scatter plot'
    plot(labeled[, features.selection.player[1:3]], main=main, col=labeled$label)
}, '../output/exploring-scatter-plot-player')

render_plot(function () {
    main = 'Exploring - Scatter plot'
    plot(labeled.team[, features.selection.team[1:3]], main=main, col=labeled.team$label)
}, '../output/exploring-scatter-plot-team')

# Only winners
render_plot(function () {
    main = 'Exploring - Scatter plot winners'
    plot(winners[, features.selection.player[1:3]], main=main, col=winners$label)
}, '../output/exploring-scatter-plot-winners-player')

render_plot(function () {
    main = 'Exploring - Scatter plot winners'
    plot(winners.team[, features.selection.team[1:3]], main=main, col=winners.team$label)
}, '../output/exploring-scatter-plot-winners-team')

# Only losers
render_plot(function () {
    main = 'Exploring - Scatter plot losers'
    plot(losers[, features.selection.player[1:3]], main=main, col=losers$label)
}, '../output/exploring-scatter-plot-losers-player')

render_plot(function () {
    main = 'Exploring - Scatter plot losers'
    plot(losers.team[, features.selection.team[1:3]], main=main, col=losers.team$label)
}, '../output/exploring-scatter-plot-losers-player')

# 3-D visualization of 3 principal components of the labeled data
render_plot(function () {
    par(mfrow=c(1, 3))
    lim = c(-2, 2)
    angle = 0
    pca_plot(labeled[, features.selection.player[1:3]], main='PCA', color=labeled$label,
             angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(winners[, features.selection.player[1:3]], main='PCA winners',
             color=winners$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(losers[, features.selection.player[1:3]], main='PCA losers',
             color=losers$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
}, '../output/exploring-pca-player', width=18, height=9)

render_plot(function () {
    par(mfrow=c(1, 3))
    lim = c(-0.1, 0.1)
    angle = 95
    features = features.selection.team
    pca_plot(labeled.team[, features], main='PCA', color=labeled.team$label,
             angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(winners.team[, features], main='PCA winners',
             color=winners.team$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
    pca_plot(losers.team[, features], main='PCA losers',
             color=losers.team$label, angle=angle, xlim=lim, ylim=lim, zlim=lim)
}, '../output/exploring-pca-team', width=18, height=9)

# In general, we can observe the k clusters found in k-means clustering. We can
# also observe that some clusters are more perceptible than others when the
# labeled data is discriminated between winners and losers.

########################
# TODO Centroid analysis
########################

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
    lim = c(-0.02, 0.02)
    plot_by(labeled.team[, features.selection.team], labeled.team$label, mean, ylim=lim)
    plot_by(winners.team[, features.selection.team], winners.team$label, mean, ylim=lim)
    plot_by(losers.team[, features.selection.team], losers.team$label, mean, ylim=lim)
}, '../output/exploring-centers-team', width=16, height=9)

# TODO Relevant feature selection analysis ............................................................................
# feature selection to quantify the discriminative power of attributes.
# In order to perform feature selection, a number of different measures are used in order to quantify the relevance of
# a feature (its discriminative power) to the classification process.
# References:
# - Data Classification Algorithms and Applications (2015)
# - COMPARISON OF FILTER BASED FEATURE SELECTION ALGORITHMS: AN OVERVIEW
# - https://pdfs.semanticscholar.org/8adc/91eb8713fdef1ac035d2832990457eec4868.pdf
player.sampled = as.data.frame(player[rownames(player) %in% rownames(labeled), ])
team.sampled = team[rownames(team) %in% rownames(labeled.team), ]

each(function (k) {
    plot_name = strf('../output/correlation-team-%s', k)
    render_plot(function () {
        correlation_analysis(labeled.team[labeled.team$label == k, features.selection.team])$estimates
    }, plot_name, width=18, height=12)
}, sort(unique(team.sampled$label)))

import_package('caret', attach=TRUE)
import_package('logistf', attach=TRUE)

train_clusters = function (data, features, target, label, method) {
    data = if (!is.data.frame(data)) as.data.frame(data) else data
    data[, target] = as.factor(data[, target])
    data = data[, c(features, target, label)]

    result = Map(function (k) {
        cluster = data[data[, label] == k, c(features, target)]

        # remove redundant features
        redundant_features = redundant_features(cluster[, features])
        features = setdiff(features, redundant_features)

        # remove zero variance features, ie, features with a single 'class'
        zero_variance_features = filter_features(cluster[, features], function(y) {
            return(if (is.integer(y)) length(unique(y)) else NULL)
        }, max=1)
        features = setdiff(features, zero_variance_features)

        # select features using feature selection method (score handler) and criteria handler
        features = feature_selection(cluster, target, function (x) {
            method(as.formula(strf('%s ~ .', target)), x)
        }, criteria_handler=function (x) x > 0)$features

        cluster = cluster[, c(features, target)]

        # render_plot(function () {
        #     correlation_analysis(cluster[, features])
        # }, strf('../output/correlation-team-%s', k), width=18, height=12)

        partitions = caret::createDataPartition(cluster[, target], p=0.6, list=FALSE)
        training = as.data.frame(cluster[partitions, ])
        testing = as.data.frame(cluster[-partitions, ])

        model = train(as.formula(strf('%s ~ .', target)), data=training, method="glm", family="binomial")

        predicted = predict(model, newdata=testing[, features, drop=FALSE])
        accuracy = table(predicted, testing[, target])
        score = sum(diag(accuracy))/sum(accuracy)

        return(list(
            k=k,
            redundant_features=redundant_features,
            zero_variance_features=zero_variance_features,
            features=features,
            model=model,
            accuracy=accuracy,
            score=score
        ))

    }, sort(unique(data[, label])))

    return(result)
}

train_clusters(team.sampled, setdiff(features.selection.team, c('')),  'winner', 'label', FSelector::information.gain)

relevant_features.information_gain = information_gain(data.sampled, features.selection.player, 'winner', 'label', criteria_handler=function (x) x > 0)
relevant_features.team.information_gain = information_gain(team.sampled, features.selection.team, 'winner', 'label', criteria_handler=function (x) x > 0)

relevant_features.gini = gini(data.sampled, features.selection.player, 'winner', 'label', criteria_handler=function (x) x < 0.7)
relevant_features.team.gini = gini(team.sampled, features.selection.team, 'winner', 'label', criteria_handler=function (x) x < 0.7)

relevant_features.relieff = relieff(data.sampled, features.selection.player, 'winner', 'label', criteria_handler=function (x) x > 0)
relevant_features.team.relieff = relieff(team.sampled, features.selection.team, 'winner', 'label', criteria_handler=function (x) x > 0)

relevant_features.random_forest = random.forest.importance(data.sampled, features.selection.player, 'winner', 'label', criteria_handler=function (x) x > apply(x, 1, mean))
relevant_features.team.random_forest = random.forest.importance(team.sampled, features.selection.team, 'winner', 'label', criteria_handler=function (x) x > apply(x, 1, mean))

# TODO remove features present in all clusters

# TODO correlation analysis by cluster to remove redundant ones

# intersection
(relevant_features.information_gain$is_selected) * (relevant_features.gini$is_selected) * (relevant_features.relieff$is_selected)
(relevant_features.team.information_gain$is_selected) * (relevant_features.team.gini$is_selected) * (relevant_features.team.relieff$is_selected)

# random forest score > mean
tmp = relevant_features.random_forest$is_selected
mode(tmp) <- 'numeric'

# TODO Classification [winner x loser] .................................................................................................

team.sampled

import_package('caret', attach=TRUE)
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
(function (data, features) {
    cluster = 'label'
    cluster_value = '1'
    target = 'winner'

    features = features[[strf('%s%s', cluster, cluster_value)]]

    data = as.data.frame(data[data[, cluster] == cluster_value, c(features, target), ])
    data[, target] = as.factor(data[, target])

    correlation_analysis(data[, features])
    features.redundant = redundant_features(data[, features])
    features = setdiff(features, features.redundant)

    partitions = caret::createDataPartition(data[, target], p=0.6, list=FALSE)
    training = as.data.frame(data[partitions, ])
    testing = as.data.frame(data[-partitions, ])

    formula = as.formula(strf('%s ~ .', target))
    model = train(formula, data=training, method="glm", family="binomial")

    predicted = predict(model, newdata=testing[, features, drop=FALSE])
    accuracy = table(predicted, testing[, target])
    return(sum(diag(accuracy))/sum(accuracy))
})(team.sampled, relevant_features.team.information_gain$selection)

# TODO feature selection using random forest
# http://stats.stackexchange.com/questions/56092/feature-selection-packages-in-r-which-do-both-regression-and-classification
# https://cran.r-project.org/web/packages/varSelRF/varSelRF.pdf

# TODO
# http://stats.stackexchange.com/questions/56092/feature-selection-packages-in-r-which-do-both-regression-and-classification
# http://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
