options(scipen=999)
options("width"=120)

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

#' Remove AFK players
remove_afk_players = function (data) {
    return(data[!(data$totalDamageDealtToChampions == 0 | data$goldSpent <= 500 | data$matchDuration < 20), ])
}

# =========
# Load data
# =========

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
# data = read.csv('../data/data.csv')
data = read.csv('../data/data20170105025503.csv')

# nrow(data)
# [1] 1100000

# Remove some games with error or AFK players
# Remove inconsistent matches, i.e. matches not 5x5
data = get_5x5_matches(data)
data = remove_afk_players(data)

# > nrow(data)
# [1] 1049423

# Remove duplicated rows by summonerId to decrease bias
# data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)
# [1] 54681

# ==================
# Feature extraction
# ==================
data[, 'minionsKilledEnemyTeam'] = data[, 'minionsKilled'] - data[, 'neutralMinionsKilled']
data[, 'physicalDamageDealtToMonsters'] = data[, 'physicalDamageDealt'] - data[, 'physicalDamageDealtToChampions']
data[, 'magicDamageDealtToMonsters'] = data[, 'magicDamageDealt'] - data[, 'magicDamageDealtToChampions']

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
# [1] "assists"                         "deaths"                          "doubleKills"
# [4] "goldEarned"                      "goldSpent"                       "inhibitorKills"
# [7] "killingSprees"                   "kills"                           "largestCriticalStrike"
# [10] "largestKillingSpree"             "largestMultiKill"                "magicDamageDealt"
# [13] "magicDamageDealtToChampions"     "magicDamageTaken"                "minionsKilled"
# [16] "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [19] "pentaKills"                      "physicalDamageDealt"             "physicalDamageDealtToChampions"
# [22] "physicalDamageTaken"             "quadraKills"                     "sightWardsBoughtInGame"
# [25] "totalDamageDealt"                "totalDamageDealtToChampions"     "totalDamageTaken"
# [28] "totalHeal"                       "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [31] "towerKills"                      "tripleKills"                     "trueDamageDealt"
# [34] "trueDamageDealtToChampions"      "trueDamageTaken"                 "visionWardsBoughtInGame"
# [37] "wardsKilled"                     "wardsPlaced"                     "minionsKilledEnemyTeam"
# [40] "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

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
# [1] "doubleKills"             "inhibitorKills"          "killingSprees"           "largestKillingSpree"
# [5] "largestMultiKill"        "pentaKills"              "quadraKills"             "sightWardsBoughtInGame"
# [9] "towerKills"              "tripleKills"             "visionWardsBoughtInGame"

# Pre-select features with relevant variance
features.selection = setdiff(features.numeric, features.low_variance)
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealt"                "magicDamageDealtToChampions"     "magicDamageTaken"
# [10] "minionsKilled"                   "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle"
# [13] "neutralMinionsKilledTeamJungle"  "physicalDamageDealt"             "physicalDamageDealtToChampions"
# [16] "physicalDamageTaken"             "totalDamageDealt"                "totalDamageDealtToChampions"
# [19] "totalDamageTaken"                "totalHeal"                       "totalTimeCrowdControlDealt"
# [22] "totalUnitsHealed"                "trueDamageDealt"                 "trueDamageDealtToChampions"
# [25] "trueDamageTaken"                 "wardsKilled"                     "wardsPlaced"
# [28] "minionsKilledEnemyTeam"          "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

# ======================
# Treatment of outliers
# ======================

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features.
outliers = render_plot(function () {
    return(outlier_analysis(data[, features.selection], factor=3))
}, '../output/outliers-for-each-one', width=16, height=12)
# outliers$total
#> [1] 247099
# > t(outliers$thresholds)
#                                     lower  upper
# assists                             -16.0     33
# deaths                               -8.0     20
# goldEarned                        -6209.0  30254
# goldSpent                         -6410.0  28345
# kills                               -15.0     27
# largestCriticalStrike             -1296.0   1728
# magicDamageDealt                -169387.0 244152
# magicDamageDealtToChampions      -29601.0  44074
# magicDamageTaken                 -14970.0  31874
# minionsKilled                      -376.0    604
# neutralMinionsKilled                -62.0     85
# neutralMinionsKilledEnemyJungle     -15.0     20
# neutralMinionsKilledTeamJungle      -51.0     68
# physicalDamageDealt             -276922.0 401511
# physicalDamageDealtToChampions   -36535.0  52337
# physicalDamageTaken              -20312.0  48400
# totalDamageDealt                -229762.0 457148
# totalDamageDealtToChampions      -34614.0  70120
# totalDamageTaken                 -27117.0  74593
# totalHeal                        -12030.0  20135
# totalTimeCrowdControlDealt        -1297.0   2014
# totalUnitsHealed                     -5.0      9
# trueDamageDealt                  -22185.0  30476
# trueDamageDealtToChampions        -3420.0   4560
# trueDamageTaken                   -2738.0   4318
# wardsKilled                          -9.0     12
# wardsPlaced                         -13.0     36
# minionsKilledEnemyTeam             -426.0    624
# physicalDamageDealtToMonsters   -238109.0 345908
# magicDamageDealtToMonsters      -143753.5 203023

# Select only features where lower != upper from outlier analysis
features.selection = colnames(outliers$thresholds)
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealt"                "magicDamageDealtToChampions"     "magicDamageTaken"
# [10] "minionsKilled"                   "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle"
# [13] "neutralMinionsKilledTeamJungle"  "physicalDamageDealt"             "physicalDamageDealtToChampions"
# [16] "physicalDamageTaken"             "totalDamageDealt"                "totalDamageDealtToChampions"
# [19] "totalDamageTaken"                "totalHeal"                       "totalTimeCrowdControlDealt"
# [22] "totalUnitsHealed"                "trueDamageDealt"                 "trueDamageDealtToChampions"
# [25] "trueDamageTaken"                 "wardsKilled"                     "wardsPlaced"
# [28] "minionsKilledEnemyTeam"          "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

# remove extreme outliers
data = data[!outliers$outliers, ]
# nrow(data)
#> [1] 802324

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent (incomplete) matches need to be removed.
data = get_5x5_matches(data)
# nrow(data)
#> [1] 181880

# ==================
# Data normalization
# ==================

# As the match duration varies between the matches, the features were divided
# by match duration to compute players performance per minute.
data.performance = cbind(data[, features.numeric]/data[, 'matchDuration'], data[, c('matchId', 'winner')])

# team performance per minute
team.performance = aggregate(. ~ matchId + winner, data=data.performance,  FUN=sum)

# What is the player performance in relation to the team?
# As the player performance varies between the matches and the features also are of different varieties, the player
# performance was divided by team performance to compute relative performance, wich can ranges from 0 to 1, and thus
# scale up the data in a uniform way. Relative performance of the players in their respective teams:
data.relative_performance = as.data.frame(t(map(function (i) {
    x = data.performance[i, ]
    t = team.performance[team.performance$matchId == x$matchId & team.performance$winner == x$winner, ]
    x[, features.numeric] = x[, features.numeric]/t[, features.numeric]
    return(x)
}, 1:nrow(data.performance))))
rownames(data.relative_performance) = rownames(data.performance)

team.performance = sapply(team.performance, as.numeric)
team.performance[is.nan(team.performance)] <- 0
team.performance = as.data.frame(team.performance)
team = as.data.frame(team.performance[, c(features.numeric, 'matchId', 'winner')])

data.relative_performance = sapply(data.relative_performance, as.numeric)
data.relative_performance[is.nan(data.relative_performance)] <- 0
data.relative_performance = as.data.frame(data.relative_performance)

data.normalized = data.relative_performance

# Since the team performances are of different varieties, their scales are also
# different. In order to maintain uniform scalability we normalize the features.
team.normalized = as.data.frame(cbind(
    normalize(team.performance[, features.numeric]),
    team.performance[, c('matchId', 'winner')]
))

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
# [1] "totalDamageDealt"                "goldEarned"                      "goldSpent"
# [4] "totalDamageDealtToChampions"     "neutralMinionsKilledEnemyJungle" "kills"
# [7] "assists"                         "physicalDamageDealt"             "deaths"
# [10] "physicalDamageDealtToMonsters"   "minionsKilled"                   "neutralMinionsKilled"
# [13] "physicalDamageDealtToChampions"  "minionsKilledEnemyTeam"          "totalHeal"
# [16] "magicDamageDealt"                "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"
# [19] "trueDamageDealt"                 "wardsPlaced"                     "totalDamageTaken"
# [22] "totalTimeCrowdControlDealt"      "neutralMinionsKilledTeamJungle"  "largestCriticalStrike"
# [25] "trueDamageDealtToChampions"      "magicDamageTaken"                "physicalDamageTaken"
# [28] "wardsKilled"                     "totalUnitsHealed"                "trueDamageTaken"

# Compound features
features.compound = c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions', # physicalDamageDealtToChampions + magicDamageDealtToChampions
    'totalDamageTaken', # physicalDamageTaken + magicDamageTaken
    'neutralMinionsKilled', # neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle
    'minionsKilled', #  neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle + minionsKilledEnemyTeam
    'trueDamageDealt', # trueDamageDealtToChampions + trueDamageDealtToMonsters
    'physicalDamageDealt', # physicalDamageDealtToChampions + physicalDamageDealtToMonsters
    'magicDamageDealt' # magicDamageDealtToChampions + magicDamageDealtToMonsters
)

# Remove compound feaures (leaving only atomic attributes - first normal form)
features.selection = setdiff(features.selection, features.compound)

# Redundant features
features.redundant.player = redundant_features(data.normalized[, features.selection], threshold=0.65)
# [1] "goldEarned"                    "physicalDamageDealtToMonsters" "magicDamageDealtToChampions"
# [4] "goldSpent"

features.redundant.team = redundant_features(team.normalized[, features.selection], threshold=0.65)
# [1] "goldEarned"                    "kills"                         "goldSpent"
# [4] "physicalDamageDealtToMonsters" "magicDamageDealtToChampions"

# Remove redundant features: highly similarity (dendogram) and correlation (heatmap).
features.selection.player = setdiff(features.selection, features.redundant.player)
# [1] "neutralMinionsKilledEnemyJungle" "kills"                           "assists"
# [4] "deaths"                          "physicalDamageDealtToChampions"  "minionsKilledEnemyTeam"
# [7] "totalHeal"                       "magicDamageDealtToMonsters"      "wardsPlaced"
# [10] "totalTimeCrowdControlDealt"      "neutralMinionsKilledTeamJungle"  "largestCriticalStrike"
# [13] "trueDamageDealtToChampions"      "magicDamageTaken"                "physicalDamageTaken"
# [16] "wardsKilled"                     "totalUnitsHealed"                "trueDamageTaken"

features.selection.team = setdiff(features.selection, c(features.redundant.team, 'deaths'))
# [1] "neutralMinionsKilledEnemyJungle" "assists"                         "deaths"
# [4] "physicalDamageDealtToChampions"  "minionsKilledEnemyTeam"          "totalHeal"
# [7] "magicDamageDealtToMonsters"      "wardsPlaced"                     "totalTimeCrowdControlDealt"
# [10] "neutralMinionsKilledTeamJungle"  "largestCriticalStrike"           "trueDamageDealtToChampions"
# [13] "magicDamageTaken"                "physicalDamageTaken"             "wardsKilled"
# [16] "totalUnitsHealed"                "trueDamageTaken"

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
    return(cluster_analysis(data.normalized[, features.selection.player], kmax=60)$fits)
}, '../output/k-means-error-curve-player')

fits.team = render_plot(function () {
    return(cluster_analysis(team.normalized[, features.selection.team], kmax=60)$fits)
}, '../output/k-means-error-curve-team')

# render_plot(function () {
#     return(bagging_cluster_analysis(data.normalized[, features.selection.player], kmax=120, ntests=10))
# }, '../output/k-means-error-curve-player-avaraged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(data.normalized, kmax=120, ncol=ncol(data.normalized[, features.selection.player]), ntests=10))
# }, '../output/k-means-error-curve-player-bagging')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.normalized[, features.selection.team], kmax=120, ntests=10))
# }, '../output/k-means-error-curve-team-avareged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.normalized, kmax=120, ncol=ncol(team.normalized[, features.selection.team]), ntests=10))
# }, '../output/k-means-error-curve-team-bagging')

# Which is the optimal fit in this case? Analysing the error curve plot, the
# k = 7 fit seems to have the best trade-off, as the rate difference does not
# vary so much after it.
fit = fits[[6]]
# each(function (i) write.csv(fit[i], strf('../output/fit/%s.csv', i)), names(fit))

fit.team = fits.team[[5]]

# Labeling data
data = cbind(data, label=fit$cluster)
player = cbind(data.performance, label=fit$cluster)

team = cbind(team.performance, label=fit.team$cluster)
team.normalized = cbind(team.normalized, label=fit.team$cluster)

labeled = cbind(winner=data[, 'winner'], data[, features.info], data.normalized[, features.selection.player], label=fit$cluster)
labeled.team = cbind(label=fit.team$cluster, team.normalized[, c(features.selection.team, 'matchId', 'winner')])

###############################################################################
# Clustered data balancing (undersampling) by discriminating winners and losers
###############################################################################

# Undersampling clustered data based on min size
labeled = balance(labeled, 'winner', 'label')$data
# $size
#     all winners losers
# 1 28247   14636  13611
# 2 35868   17961  17907
# 3 35923   18186  17737
# 4 29894   14157  15737
# 5 16495    7795   8700
# 6 35453   18205  17248
#
# $relative_size
#     winners    losers
# 1 0.5181435 0.4818565
# 2 0.5007528 0.4992472
# 3 0.5062495 0.4937505
# 4 0.4735733 0.5264267
# 5 0.4725674 0.5274326
# 6 0.5134967 0.4865033
#
# $min_size
# [1] 7795

labeled.team = balance(labeled.team, 'winner', 'label')$data
# $size
#     all winners losers
# 1  9888    5516   4372
# 2 10402    4315   6087
# 3  6500    1731   4769
# 4  6955    4646   2309
# 5  2631    1980    651
#
# $relative_size
#     winners    losers
# 1 0.5578479 0.4421521
# 2 0.4148241 0.5851759
# 3 0.2663077 0.7336923
# 4 0.6680086 0.3319914
# 5 0.7525656 0.2474344
#
# $min_size
# [1] 651

##########################################
# TODO Statistical analysis of the results
##########################################

(function (data, features) {
    winners = data[data$winner == 1, ]
    losers = data[data$winner == 0, ]

    # Hypothesis 1. H1-0: There is no difference between the distributions of the
    # clusters found in the learning model; H1-1 There is difference between the
    # distributions of the clusters found in the learning model. Test:
    # Kruskal-Wallis rank sum test

    # Alternative hypothesis true: p.value < 0.05
    h1 = kruskal.test(rowSums(data[, features]), data$label)

    # Hypothesis 2. H2-0: For each cluster found in the learning model there is no
    # difference between the medians of the winning players and losing players;
    # (H2-1) for each cluster found there is difference between the medians of the
    # winning players and losing players. Test: Wilcoxon rank sum test with
    # continuity correction

    # Alternative hypothesis true for each cluster: p.value < 0.05
    h2.p.values = values(Map(function (k) {
        x = rowSums(winners[winners$label == k, features])
        y = rowSums(losers[losers$label == k, features])
        # TODO print(paste(shapiro.test(x)$p.value, shapiro.test(y)$p.value))
        # TODO print(t.test(x, y, paired=FALSE)$p.value)
        wilcox.test(x , y, paired=FALSE)$p.value
    }, sort(unique(data$label))))

    render_plot(function () {
        par(mfrow=c(1, 2))
        plot(1, h1$p.value, main='Hypothesis - H1', xlab='h1', ylab='p.value')
        plot(h2.p.values, main='Hypothesis - H2', xlab='k', ylab='p.values')
    }, '../output/hypothesis-player', width=16, height=9)

    # TODO compare median between clusters
})(labeled, features.selection.player)

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

centroid_analysis(labeled, features.selection.player, '../output/exploring-centers-player')

centroid_analysis(labeled.team, setdiff(features.selection.team, 'magicDamageDealtToMonsters'), '../output/exploring-centers-team')

##########################################
# TODO Relevant feature selection analysis
##########################################

# feature selection to quantify the discriminative power of attributes.
# In order to perform feature selection, a number of different measures are used in order to quantify the relevance of
# a feature (its discriminative power) to the classification process.
# References:
# - Data Classification Algorithms and Applications (2015)
# - COMPARISON OF FILTER BASED FEATURE SELECTION ALGORITHMS: AN OVERVIEW
# - https://pdfs.semanticscholar.org/8adc/91eb8713fdef1ac035d2832990457eec4868.pdf

import_package('caret', attach=TRUE)
import_package('logistf', attach=TRUE)

training = balance(team, 'winner', 'label')$data
testing = team[!(rownames(team) %in% rownames(training)), ]

# > nrow(training)
# [1] 6510
# > nrow(testing)
# [1] 29866

train_clusters = function (data, features, target, label, feature_selector) {
    data = if (!is.data.frame(data)) as.data.frame(data) else data
    data = data[, c(features, target, label)]

    # classes can't be numeric
    data[, target] = as.factor(data[, target])

    label_values = sort(unique(data[, label]))
    label_names = Map(function (i) strf('%s%s', label, i), label_values)

    result = Map(function (k) {
        cluster = data[data[, label] == k, c(features, target)]
        features = feature_engeneering(cluster, features, target)
        cluster = cluster[, c(features, target)]

        # render_plot(function () {
        #     correlation_analysis(cluster[, features])
        # }, strf('../output/correlation-team-%s', k), width=18, height=12)

        partitions = caret::createDataPartition(cluster[, target], p=0.6, list=FALSE)
        training = as.data.frame(cluster[partitions, ])
        validation = as.data.frame(cluster[-partitions, ])

        model = train(as.formula(strf('%s ~ .', target)), data=training, method="glm", family="binomial")

        predicted = predict(model, newdata=validation[, features, drop=FALSE])
        confusion_matrix = table(predicted, validation[, target])
        accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)

        return(list(
            k=k,
            label=label,
            features=features,
            model=model,
            confusion_matrix=confusion_matrix, # TODO precision and recall
            accuracy=accuracy
        ))

    }, label_values)
    names(result) = label_names

    return(result)
}

tcr = train_clusters(training, setdiff(c(features.selection.team), c('deaths')),  'winner', 'label', FSelector::information.gain)
Map(function(i) i$accuracy, tcr)
# $label1
# [1] 0.9326923
#
# $label2
# [1] 0.9596154
#
# $label3
# [1] 0.9596154
#
# $label4
# [1] 0.9557692
#
# $label5
# [1] 0.9596154

testing_clusters = function (testing, target, label) {
    return(Map(function (i) {
        label_value = i$k
        label_name = i
        model = i$model
        features = i$features
        testing = testing
        testing = testing[testing[, label] == label_value, ]
        # target_values = as.numeric(as.factor(testing[, target])) # 0 1 -> 1 2
        target_values = as.factor(testing[, target])

        predicted_prob = predict(model, newdata=testing[, features, drop=FALSE], type="prob")

        predicted_prob = sapply(1:nrow(testing), function (i) {
            return(predicted_prob[i, target_values[i]])
        })

        predicted = sapply(1:nrow(testing), function (i) {
            target = target_values[i]
            if (predicted_prob[i] > 0.5 & target == 0 || predicted_prob[i] < 0.5 & target == 1)
                return(0)
            else
                return(1)
        })

        # print('target_values')
        # print(table(target_values))
        # print(head(target_values))
        # print('target_values')
        # print(table(predicted))
        # print(head(predicted))
        # print(head(cbind(target_values, predicted_prob, predicted)))

        accuracy = table(predicted, target_values)
        # print(accuracy)
        score = sum(diag(accuracy))/sum(accuracy)

        return(list(score=score, accuracy=accuracy, predicted=predicted_prob, testing=target_values))
    }, tcr))
}

xxx = testing_clusters(testing, 'winner', 'label')

Map(function(i) i$score, xxx)
# $label1
# [1] 0.9367575
#
# $label2
# [1] 0.9481319
#
# $label3
# [1] 0.9665256
#
# $label4
# [1] 0.9571909
#
# $label5
# [1] 0.9495862

install.packages('ROCR', dependencies=TRUE)
import_package('ROCR', attach=TRUE)
# TODO library(ROCR)

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
