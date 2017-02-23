options(scipen=999)
options("width"=120)

library('modules')
import('fun', attach=TRUE)
import('outliers', attach=TRUE)
import('feature_selection', attach=TRUE)
import('utils', attach=TRUE)

RENDER_PLOT_SAVE = TRUE
RENDER_PLOT_CLOSE = TRUE

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
data = remove_afk_players(data)
data = get_5x5_matches(data)

# > nrow(data)
# [1] 1036090

# TODO Remove duplicated rows by summonerId to decrease bias
# data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)

# ===================
# Features definition
# ===================

features.info = c(
    'matchId', 'matchMode', 'queueType', 'season', 'championId', 'summonerId',
    'matchDuration', # In minutes
    'matchCreation', 'matchCreationYear', 'matchCreationMonth', 'matchCreationDay', 'matchCreationHour',
    'item0', 'item1', 'item2', 'item3', 'item4', 'item5', 'item6',
    'combatPlayerScore', 'objectivePlayerScore', 'totalPlayerScore', 'totalScoreRank', 'unrealKills'  # ARAM
)

features.target = 'winner'

features.logical = c(
    # 'firstBloodAssist', 'firstBloodKill', 'firstTowerAssist', 'firstTowerKill'
    'firstInhibitorAssist', 'firstInhibitorKill'
)

features.categorical = c('champLevel')

features.numeric = sort(setdiff(names(data), c(features.target, features.info, features.logical, features.categorical)))
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
# [37] "wardsKilled"                     "wardsPlaced"

features.ong = c(
    'firstBloodKill', 'firstTowerKill', 'firstTowerAssist', 'kills', 'assists', 'deaths', 'goldEarned',
    'totalDamageDealt', 'magicDamageDealt', 'physicalDamageDealt', 'totalDamageDealtToChampions', 'totalDamageTaken',
    'minionsKilled', 'neutralMinionsKilled', 'totalTimeCrowdControlDealt', 'wardsPlaced', 'towerKills',
    'largestMultiKill', 'largestKillingSpree', 'largestCriticalStrike', 'totalHeal'
)

features.ong.numeric = c(
    'goldEarned', 'kills', 'physicalDamageDealt', 'minionsKilled', 'totalDamageTaken',
    'towerKills', 'largestCriticalStrike', 'neutralMinionsKilled', 'assists', 'totalTimeCrowdControlDealt',
    'magicDamageDealt', 'wardsPlaced', 'totalHeal', 'deaths'
)

# =========================================
# Feature extraction: Feature decomposition
# =========================================
data[, 'minionsKilledEnemyTeam'] = data[, 'minionsKilled'] - data[, 'neutralMinionsKilled']
data[, 'physicalDamageDealtToMonsters'] = data[, 'physicalDamageDealt'] - data[, 'physicalDamageDealtToChampions']
data[, 'magicDamageDealtToMonsters'] = data[, 'magicDamageDealt'] - data[, 'magicDamageDealtToChampions']

# ========================
# Simple feature selection
# ========================

features.numeric = sort(setdiff(names(data), c(features.target, features.info, features.logical, features.categorical)))
# [1] "assists"                         "deaths"                          "doubleKills"
# [4] "goldEarned"                      "goldSpent"                       "inhibitorKills"
# [7] "killingSprees"                   "kills"                           "largestCriticalStrike"
# [10] "largestKillingSpree"             "largestMultiKill"                "magicDamageDealt"
# [13] "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"      "magicDamageTaken"
# [16] "minionsKilled"                   "minionsKilledEnemyTeam"          "neutralMinionsKilled"
# [19] "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"  "pentaKills"
# [22] "physicalDamageDealt"             "physicalDamageDealtToChampions"  "physicalDamageDealtToMonsters"
# [25] "physicalDamageTaken"             "quadraKills"                     "sightWardsBoughtInGame"
# [28] "totalDamageDealt"                "totalDamageDealtToChampions"     "totalDamageTaken"
# [31] "totalHeal"                       "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [34] "towerKills"                      "tripleKills"                     "trueDamageDealt"
# [37] "trueDamageDealtToChampions"      "trueDamageTaken"                 "visionWardsBoughtInGame"
# [40] "wardsKilled"                     "wardsPlaced"

# Remove features with low variance
features.selection = setdiff(features.numeric, filter_features(data[, features.numeric], var, max=8))
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealt"                "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"
# [10] "magicDamageTaken"                "minionsKilled"                   "minionsKilledEnemyTeam"
# [13] "neutralMinionsKilled"            "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [16] "physicalDamageDealt"             "physicalDamageDealtToChampions"  "physicalDamageDealtToMonsters"
# [19] "physicalDamageTaken"             "totalDamageDealt"                "totalDamageDealtToChampions"
# [22] "totalDamageTaken"                "totalHeal"                       "totalTimeCrowdControlDealt"
# [25] "totalUnitsHealed"                "trueDamageDealt"                 "trueDamageDealtToChampions"
# [28] "trueDamageTaken"                 "wardsKilled"                     "wardsPlaced"

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
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"      "magicDamageTaken"
# [10] "minionsKilledEnemyTeam"          "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [13] "physicalDamageDealtToChampions"  "physicalDamageDealtToMonsters"   "physicalDamageTaken"
# [16] "totalHeal"                       "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [19] "trueDamageDealtToChampions"      "trueDamageTaken"                 "wardsKilled"
# [22] "wardsPlaced"

# ================
# Outlier analysis
# ================

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features
outliers = render_plot(function () {
    return(outlier_analysis(data[, features.selection], factor=3))
}, '../output/outliers-for-each-one', width=16, height=12)
# outliers$total
#> [1] 240702
# > t(outliers$thresholds)
#                                   lower  upper
# assists                             -19     37
# deaths                               -8     20
# goldEarned                        -6133  30239
# goldSpent                         -6325  28325
# kills                               -15     27
# largestCriticalStrike             -1302   1736
# magicDamageDealtToChampions      -29717  44252
# magicDamageDealtToMonsters      -144318 203813
# magicDamageTaken                 -14932  31898
# minionsKilledEnemyTeam             -429    628
# neutralMinionsKilledEnemyJungle     -15     20
# neutralMinionsKilledTeamJungle      -51     68
# physicalDamageDealtToChampions   -36712  52580
# physicalDamageDealtToMonsters   -239124 347273
# physicalDamageTaken              -20299  48469
# totalHeal                        -12051  20191
# totalTimeCrowdControlDealt        -1299   2019
# totalUnitsHealed                     -5      9
# trueDamageDealtToChampions        -3432   4576
# trueDamageTaken                   -2742   4328
# wardsKilled                          -9     12
# wardsPlaced                         -13     36

# Select only features where lower != upper from outlier analysis
features.selection = colnames(outliers$thresholds)
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"      "magicDamageTaken"
# [10] "minionsKilledEnemyTeam"          "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [13] "physicalDamageDealtToChampions"  "physicalDamageDealtToMonsters"   "physicalDamageTaken"
# [16] "totalHeal"                       "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [19] "trueDamageDealtToChampions"      "trueDamageTaken"                 "wardsKilled"
# [22] "wardsPlaced"

# remove extreme outliers
data = data[!outliers$outliers, ]
# nrow(data)
#> [1] 795388

# Remove teams with extreme outliers
# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent (incomplete) matches need to be removed.
data = get_5x5_matches(data)
# nrow(data)
#> [1] 187450

# ===================
# Data transformation
# ===================

# As the match duration varies between the matches, the features were divided
# by match duration to compute players performance per minute.
data.performance = cbind(data[, features.numeric]/data[, 'matchDuration'], data[, c('matchId', 'winner')])

# team performance per minute
team.performance = aggregate(. ~ matchId + winner, data=data.performance,  FUN=sum)

# ==================
# Data normalization
# ==================

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

# ===================================
# Correlation and similarity analysis
# ===================================

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
# [1] "goldEarned"                      "goldSpent"                       "kills"
# [4] "assists"                         "neutralMinionsKilledEnemyJungle" "deaths"
# [7] "physicalDamageDealtToMonsters"   "physicalDamageDealtToChampions"  "totalHeal"
# [10] "minionsKilledEnemyTeam"          "magicDamageDealtToChampions"     "magicDamageDealtToMonsters"
# [13] "wardsPlaced"                     "totalTimeCrowdControlDealt"      "largestCriticalStrike"
# [16] "trueDamageDealtToChampions"      "neutralMinionsKilledTeamJungle"  "magicDamageTaken"
# [19] "physicalDamageTaken"             "wardsKilled"                     "totalUnitsHealed"
# [22] "trueDamageTaken"

# Redundant features
features.redundant.player = redundant_features(data.normalized[, features.selection], threshold=0.65)
# [1] "goldEarned"                    "physicalDamageDealtToMonsters" "magicDamageDealtToChampions"
# [4] "goldSpent"

features.redundant.team = redundant_features(team.normalized[, features.selection], threshold=0.65)
# [1] "goldEarned"                    "kills"                         "goldSpent"
# [4] "physicalDamageDealtToMonsters" "magicDamageDealtToChampions"

# Remove redundant features: highly similarity (dendogram) and correlation (heatmap).
features.selection.player = setdiff(features.selection, features.redundant.player)
# [1] "kills"                           "assists"                         "neutralMinionsKilledEnemyJungle"
# [4] "deaths"                          "physicalDamageDealtToChampions"  "totalHeal"
# [7] "minionsKilledEnemyTeam"          "magicDamageDealtToMonsters"      "wardsPlaced"
# [10] "totalTimeCrowdControlDealt"      "largestCriticalStrike"           "trueDamageDealtToChampions"
# [13] "neutralMinionsKilledTeamJungle"  "magicDamageTaken"                "physicalDamageTaken"
# [16] "wardsKilled"                     "totalUnitsHealed"                "trueDamageTaken"

features.selection.team = setdiff(features.selection, c(features.redundant.team, 'deaths'))
# [1] "assists"                         "neutralMinionsKilledEnemyJungle" "physicalDamageDealtToChampions"
# [4] "totalHeal"                       "minionsKilledEnemyTeam"          "magicDamageDealtToMonsters"
# [7] "wardsPlaced"                     "totalTimeCrowdControlDealt"      "largestCriticalStrike"
# [10] "trueDamageDealtToChampions"      "neutralMinionsKilledTeamJungle"  "magicDamageTaken"
# [13] "physicalDamageTaken"             "wardsKilled"                     "totalUnitsHealed"
# [16] "trueDamageTaken"

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

RENDER_PLOT_CLOSE = FALSE

# Perform a cluster analysis on data using k-means for each k = [1:kmax]. Also
# render a knee of the error curve plot to find the optimal k
fits = render_plot(function () {
    return(cluster_analysis(data.normalized[, features.selection.player], kmax=20)$fits)
}, '../output/k-means-error-curve-player')

fits.team = render_plot(function () {
    return(cluster_analysis(team.normalized[, features.selection.team], kmax=20)$fits)
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
fit = fits[[7]]
# each(function (i) write.csv(fit[i], strf('../output/fit/%s.csv', i)), names(fit))

fit.team = fits.team[[6]]

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
labeled = balance(labeled, 'winner', 'label', 0.8)$data
# $size
#     all winners losers
# 1 19567    6938  12629
# 2 27568   13040  14528
# 3 15614    7912   7702
# 4 35330   17536  17794
# 5 17857   11725   6132
# 6 31255   16082  15173
# 7 40259   20492  19767
#
# $relative_size
#     winners    losers
# 1 0.3545766 0.6454234
# 2 0.4730122 0.5269878
# 3 0.5067247 0.4932753
# 4 0.4963487 0.5036513
# 5 0.6566053 0.3433947
# 6 0.5145417 0.4854583
# 7 0.5090042 0.4909958
#
# $min_size
# [1] 4906

labeled.team = balance(labeled.team, 'winner', 'label')$data
# $size
#    all winners losers
# 1 9268    4692   4576
# 2 8580    3127   5453
# 3 5073    3562   1511
# 4 1853    1412    441
# 5 7875    4718   3157
# 6 4841    1234   3607
#
# $relative_size
#     winners    losers
# 1 0.5062581 0.4937419
# 2 0.3644522 0.6355478
# 3 0.7021486 0.2978514
# 4 0.7620076 0.2379924
# 5 0.5991111 0.4008889
# 6 0.2549060 0.7450940
#
# $min_size
# [1] 353

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

#######################
# TODO Predictive model
#######################

import_package('caret', attach=TRUE)
import_package('logistf', attach=TRUE)

training_set = balance(team, 'winner', 'label', prop=0.8)$data
# $size
#    all winners losers
# 1 8986    4566   4420
# 2 1812    1383    429
# 3 7607    4554   3053
# 4 4750    1197   3553
# 5 8334    3045   5289
# 6 4887    3443   1444
#
# $relative_size
#     winners    losers
# 1 0.5081237 0.4918763
# 2 0.7632450 0.2367550
# 3 0.5986591 0.4013409
# 4 0.2520000 0.7480000
# 5 0.3653708 0.6346292
# 6 0.7045222 0.2954778
#
# $min_size
# [1] 343

validation_set = team[!(rownames(team) %in% rownames(training_set)), ]

# > nrow(training_set)
# [1] 4116
# > nrow(validation_set)
# [1] 32260

#' Validate a prediction model given a validation set
validate = function (model, validation_set, features, target_feature) {
    targets = as.factor(validation_set[, target_feature])

    outcomes_prob_by_target = predict(model, newdata=validation_set[, features, drop=FALSE], type="prob")

    outcomes = sapply(1:nrow(validation_set), function (i) {
        target = targets[i]
        outcome_prob = outcomes_prob_by_target[i, target]
        if (outcome_prob > 0.5 & target == 0 || outcome_prob < 0.5 & target == 1)
            return(0)
        else
            return(1)
    })

    confusion_matrix = table(outcomes, targets)
    accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
    # TODO precision and recall
    return(list(confusion_matrix=confusion_matrix, accuracy=accuracy))
}

# partitions = caret::createDataPartition(training[, target_feature], p=0.6, list=FALSE)
# training_set = as.data.frame(training[partitions, ])
# validation_set = as.data.frame(training[-partitions, ])

#' Train and validate binary prediction model for each cluster
train_by_cluster = function (training_set, validation_set, features, target_feature, cluster_feature) {
    training_set = training_set[, c(features, target_feature, cluster_feature)]
    training_set = if (!is.data.frame(training_set)) as.data.frame(training_set) else training_set

    # target classes can't be numeric
    training_set[, target_feature] = as.factor(training_set[, target_feature])

    cluster_names = sort(unique(training_set[, cluster_feature]))

    # train and validation result for each k cluster
    cluster_results = Map(function (k) {
        # filter k cluster data
        training_set = training_set[training_set[, cluster_feature] == k, c(features, target_feature)]
        validation_set = validation_set[validation_set[, cluster_feature] == k, c(features, target_feature)]

        # perform data reduction
        features = feature_engeneering(training_set, features, target_feature)
        training_set = training_set[, c(features, target_feature)]
        validation_set = validation_set[, c(features, target_feature)]

        # logist regression modeling
        model = train(as.formula(strf('%s ~ .', target_feature)), data=training_set, method="glm", family="binomial")

        # perform validation
        validation_result = validate(model, validation_set, features, target_feature)

        # cluster result
        return(list(k=k, features=features, model=model, validation_result=validation_result))
    }, cluster_names)

    # labeling cluster result names
    names(cluster_results) = Map(function (k) strf('%s%s', cluster_feature, k), cluster_names)

    return(cluster_results)
}

train_result = train_by_cluster(training_set, validation_set, features.selection.team,  'winner', 'label')
Map(function(k) k$validation_result$accuracy, train_result)
# $label1
# [1] 0.9166083
#
# $label2
# [1] 0.9547879
#
# $label3
# [1] 0.9193955
#
# $label4
# [1] 0.9250218
#
# $label5
# [1] 0.9337425
#
# $label6
# [1] 0.9293833

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

# TODO feature selection using random forest
# http://stats.stackexchange.com/questions/56092/feature-selection-packages-in-r-which-do-both-regression-and-classification
# https://cran.r-project.org/web/packages/varSelRF/varSelRF.pdf

# TODO
# http://stats.stackexchange.com/questions/56092/feature-selection-packages-in-r-which-do-both-regression-and-classification
# http://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
