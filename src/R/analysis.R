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

#' Return matches with 5 players each team (5x5). As data were looked up by participants, some matches were left with
#' less than 10 participants. So, these inconsistent (incomplete) matches need to be removed.
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
data = get_5x5_matches(remove_afk_players(data))

# > nrow(data)
# [1] 1036090

# TODO Remove duplicated rows by summonerId to decrease bias
# data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)

# =============================
# Feature definition/extraction
# =============================

features.info = c(
    'matchId',
    'matchMode',
    'queueType',
    'season',
    'championId',
    'summonerId',
    'matchDuration', # In minutes
    'matchCreation',
    'matchCreationYear',
    'matchCreationMonth',
    'matchCreationDay',
    'matchCreationHour',
    'item0', 'item1', 'item2', 'item3', 'item4', 'item5', 'item6',
    'combatPlayerScore', 'objectivePlayerScore', 'totalPlayerScore', 'totalScoreRank', 'unrealKills'  # ARAM
)

features.target = 'winner'

features.logical = c(
    # 'firstBloodAssist', 'firstBloodKill', 'firstTowerAssist', 'firstTowerKill'
    'firstInhibitorAssist',
    'firstInhibitorKill'
)

# Categorical (nominal), ordinal or interval features
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

# ===========================================
# Feature extraction by feature decomposition
# ===========================================
# OBS: minionsKilledEnemyTeam not in minionsKilled

data[, 'physicalDamageDealtToMonsters'] = data[, 'physicalDamageDealt'] - data[, 'physicalDamageDealtToChampions']
data[, 'magicDamageDealtToMonsters'] = data[, 'magicDamageDealt'] - data[, 'magicDamageDealtToChampions']

features.numeric = c(features.numeric, 'physicalDamageDealtToMonsters', 'magicDamageDealtToMonsters')

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
features.numeric = setdiff(features.numeric, features.compound)
# [1] "assists"                         "deaths"                          "doubleKills"
# [4] "goldEarned"                      "goldSpent"                       "inhibitorKills"
# [7] "killingSprees"                   "kills"                           "largestCriticalStrike"
# [10] "largestKillingSpree"             "largestMultiKill"                "magicDamageDealtToChampions"
# [13] "magicDamageTaken"                "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [16] "pentaKills"                      "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [19] "quadraKills"                     "sightWardsBoughtInGame"          "totalHeal"
# [22] "totalTimeCrowdControlDealt"      "totalUnitsHealed"                "towerKills"
# [25] "tripleKills"                     "trueDamageDealtToChampions"      "trueDamageTaken"
# [28] "visionWardsBoughtInGame"         "wardsKilled"                     "wardsPlaced"
# [31] "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

#########################
# Descriptive statistics
#########################

# > descriptive_statistics(data, features.numeric)
#                                  min    max     mean meadian           var       sd
# assists                            0     37     6.55     5.0         25.00     5.00
# deaths                             0     20     4.97     5.0          8.74     2.96
# doubleKills                        0      9     0.47     0.0          0.75     0.86
# goldEarned                      3157  24324  9441.09  9179.0    8188521.43  2861.56
# goldSpent                        550  26778  8446.35  8200.0    7406350.05  2721.46
# inhibitorKills                     0      4     0.14     0.0          0.16     0.40
# killingSprees                      0     10     1.11     1.0          1.24     1.12
# kills                              0     27     4.94     4.0         17.41     4.17
# largestCriticalStrike              0   1736   176.28     0.0      90425.42   300.71
# largestKillingSpree                0     26     2.50     2.0          7.00     2.65
# largestMultiKill                   0      5     1.28     1.0          0.54     0.73
# magicDamageDealtToChampions        0  44247  5965.14  3701.0   41895359.03  6472.66
# magicDamageTaken                   0  31830  6015.04  5328.5   13040670.58  3611.19
# neutralMinionsKilledEnemyJungle    0     20     2.57     1.0         17.31     4.16
# neutralMinionsKilledTeamJungle     0     68    11.10     2.0        323.21    17.98
# pentaKills                         0      2     0.00     0.0          0.00     0.04
# physicalDamageDealtToChampions     0  52400  6354.05  3495.0   45975481.71  6780.52
# physicalDamageTaken               49  47836 10837.24  9961.0   28951215.06  5380.63
# quadraKills                        0      3     0.01     0.0          0.01     0.10
# sightWardsBoughtInGame             0      0     0.00     0.0          0.00     0.00
# totalHeal                          0  20183  3067.55  2100.0    9091878.88  3015.27
# totalTimeCrowdControlDealt         0   2019   292.40   187.0      97881.87   312.86
# totalUnitsHealed                   0      9     1.84     1.0          1.85     1.36
# towerKills                         0      9     0.77     0.0          1.33     1.15
# tripleKills                        0      5     0.06     0.0          0.08     0.27
# trueDamageDealtToChampions         0   4576   519.82   240.0     579497.75   761.25
# trueDamageTaken                    0   4325   541.82   382.0     309057.42   555.93
# visionWardsBoughtInGame            0     16     0.71     0.0          1.13     1.06
# wardsKilled                        0     12     1.14     1.0          2.28     1.51
# wardsPlaced                        0     36     9.24     8.0         25.84     5.08
# physicalDamageDealtToMonsters      0 328231 41916.53 27746.0 1542164943.87 39270.41
# magicDamageDealtToMonsters         0 203453 23587.58 12337.0  754640013.66 27470.71

# ==========================================
# Modeling team data from player match stats
# ==========================================

# Remove low variance features
features.selection.player = setdiff(features.numeric, filter_features(data[, features.numeric], var, max=7))
# [1] "assists"                         "deaths"                          "goldEarned"
# [4] "goldSpent"                       "kills"                           "largestCriticalStrike"
# [7] "magicDamageDealtToChampions"     "magicDamageTaken"                "neutralMinionsKilledEnemyJungle"
# [10] "neutralMinionsKilledTeamJungle"  "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [13] "totalHeal"                       "totalTimeCrowdControlDealt"      "totalUnitsHealed"
# [16] "trueDamageDealtToChampions"      "trueDamageTaken"                 "wardsKilled"
# [19] "wardsPlaced"                     "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

# ================
# Outlier analysis
# ================

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features
outliers = render_plot(function () {
    return(outlier_analysis(data[, features.selection.player], factor=3))
}, '../output/outliers-for-each-one', width=16, height=12)
# > outliers$total
# > t(outliers$thresholds)

# Remove matches with extreme outliers from player match stats data
extreme_outliers_matchIds = unique(data[outliers$outliers, 'matchId'])
data = data[!(data$matchId %in% extreme_outliers_matchIds), ]

# > nrow(data)
# [1] 187450

# Select only features where lower != upper from outlier analysis
features.selection.player = colnames(outliers$thresholds)

# Remove low variance features after outlier analyis
features.selection.player = setdiff(features.selection.player, filter_features(data[, features.selection.player], var, max=7))

# > descriptive_statistics(data, features.selection)
#                                  min    max     mean meadian           var       sd
# assists                            0     37     6.55     5.0         25.00     5.00
# deaths                             0     20     4.97     5.0          8.74     2.96
# goldEarned                      3157  24324  9441.09  9179.0    8188521.43  2861.56
# goldSpent                        550  26778  8446.35  8200.0    7406350.05  2721.46
# kills                              0     27     4.94     4.0         17.41     4.17
# largestCriticalStrike              0   1736   176.28     0.0      90425.42   300.71
# magicDamageDealtToChampions        0  44247  5965.14  3701.0   41895359.03  6472.66
# magicDamageTaken                   0  31830  6015.04  5328.5   13040670.58  3611.19
# neutralMinionsKilledEnemyJungle    0     20     2.57     1.0         17.31     4.16
# neutralMinionsKilledTeamJungle     0     68    11.10     2.0        323.21    17.98
# physicalDamageDealtToChampions     0  52400  6354.05  3495.0   45975481.71  6780.52
# physicalDamageTaken               49  47836 10837.24  9961.0   28951215.06  5380.63
# totalHeal                          0  20183  3067.55  2100.0    9091878.88  3015.27
# totalTimeCrowdControlDealt         0   2019   292.40   187.0      97881.87   312.86
# trueDamageDealtToChampions         0   4576   519.82   240.0     579497.75   761.25
# trueDamageTaken                    0   4325   541.82   382.0     309057.42   555.93
# wardsPlaced                        0     36     9.24     8.0         25.84     5.08
# physicalDamageDealtToMonsters      0 328231 41916.53 27746.0 1542164943.87 39270.41
# magicDamageDealtToMonsters         0 203453 23587.58 12337.0  754640013.66 27470.71


# ==========================================
# Modeling team data from player match stats
# ==========================================

team = aggregate(. ~ matchId + winner, data=data[, c(features.selection.player, 'winner', 'matchId')],  FUN=sum)
# > nrow(team)
# [1] 37490

# Simple descriptive statistics
# > descriptive_statistics(team, features.numeric)
#                                   min    max      mean  meadian           var       sd
# assists                             0    122     32.75     31.0        350.60    18.72
# deaths                              0     73     24.84     25.0        136.95    11.70
# doubleKills                         0     13      2.34      2.0          4.15     2.04
# goldEarned                      20603  93208  47205.47  46659.0  154273820.98 12420.70
# goldSpent                       14525  91575  42231.75  41685.0  132433329.79 11507.97
# inhibitorKills                      0      6      0.68      0.0          1.01     1.00
# killingSprees                       0     20      5.53      5.0          9.89     3.15
# kills                               0     73     24.69     24.0        136.91    11.70
# largestCriticalStrike               0   4457    881.42    775.0     325420.88   570.46
# largestKillingSpree                 0     43     12.52     12.0         53.57     7.32
# largestMultiKill                    0     14      6.38      6.0          3.43     1.85
# magicDamageDealtToChampions      1734 122100  29825.68  26985.5  227485814.93 15082.63
# magicDamageTaken                 1734 123100  30075.21  27276.5  230008427.84 15166.03
# neutralMinionsKilledEnemyJungle     0     59     12.84     11.0        107.94    10.39
# neutralMinionsKilledTeamJungle      0    145     55.48     55.0        286.36    16.92
# pentaKills                          0      2      0.01      0.0          0.01     0.09
# physicalDamageDealtToChampions   1298 131831  31770.23  29408.5  215061398.52 14664.97
# physicalDamageTaken             11711 160443  54186.21  51871.5  298809055.60 17286.09
# quadraKills                         0      3      0.04      0.0          0.05     0.21
# sightWardsBoughtInGame              0      0      0.00      0.0          0.00     0.00
# totalHeal                        1756  60022  15337.77  13999.5   53805415.18  7335.22
# totalTimeCrowdControlDealt         40   7639   1461.99   1300.0     550083.78   741.68
# totalUnitsHealed                    3     24      9.22      9.0          5.27     2.30
# towerKills                          0     12      3.87      3.0         11.34     3.37
# tripleKills                         0      5      0.31      0.0          0.38     0.61
# trueDamageDealtToChampions          0  12353   2599.11   2180.0    3047499.45  1745.71
# trueDamageTaken                     0  13001   2709.09   2282.0    3305126.80  1818.00
# visionWardsBoughtInGame             0     25      3.53      3.0          6.52     2.55
# wardsKilled                         0     49      5.70      5.0         20.14     4.49
# wardsPlaced                         8    109     46.19     45.0        191.86    13.85
# physicalDamageDealtToMonsters   28629 718083 209582.67 198049.5 7127626515.10 84425.27
# magicDamageDealtToMonsters       3492 489227 117937.89 108116.0 3582231246.48 59851.74

# Remove low variance features
features.selection.team = setdiff(features.numeric, filter_features(team[, features.numeric], var, max=7))
#  [1] "assists"                         "deaths"                          "goldEarned"
#  [4] "goldSpent"                       "killingSprees"                   "kills"
#  [7] "largestCriticalStrike"           "largestKillingSpree"             "magicDamageDealtToChampions"
# [10] "magicDamageTaken"                "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [13] "physicalDamageDealtToChampions"  "physicalDamageTaken"             "totalHeal"
# [16] "totalTimeCrowdControlDealt"      "towerKills"                      "trueDamageDealtToChampions"
# [19] "trueDamageTaken"                 "wardsKilled"                     "wardsPlaced"
# [22] "physicalDamageDealtToMonsters"   "magicDamageDealtToMonsters"

# Outlier analysis
outliers.team = render_plot(function () {
    return(outlier_analysis(team[, features.numeric], factor=3))
}, '../output/outliers-for-each-one', width=16, height=12)

# Remove matches with extreme outliers from team and player match stats data
extreme_outliers_mathIds = unique(team[outliers.team$outliers, 'matchId'])
team = team[!team$matchId %in% extreme_outliers_mathIds, ]
data = data[!data$matchId %in% extreme_outliers_mathIds, ]

# > nrow(team)
# [1] 36864

# > nrow(data)
# [1] 184320

# ===================
# Data transformation
# ===================

# As the match duration varies between the matches, the features were divided by match duration to compute players
# performance per minute.
data.performance = cbind(
    data[, c(features.info, 'matchId', 'winner')],
    data[, features.numeric]/data[, 'matchDuration']
)

# Team stats per minute
team.performance = aggregate(. ~ matchId + winner, data=data.performance,  FUN=sum)

# ==================
# Data normalization
# ==================

# What is the individual player performance in relation to his team?
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

data.relative_performance = sapply(data.relative_performance, as.numeric)
data.relative_performance[is.nan(data.relative_performance)] <- 0
data.relative_performance = as.data.frame(data.relative_performance)
# data.relative_performance = cbind(data[, setdiff(features.info, c('matchId', 'winner'))], data.relative_performance)

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
    return(correlation_analysis(data.relative_performance[, features.selection.player])$estimates)
}, '../output/correlation-player', width=18, height=12)

correlations = render_plot(function () {
    return(correlation_analysis(team.normalized[, features.selection.team])$estimates)
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
features.redundant.player = redundant_features(data.relative_performance[, features.selection], threshold=0.65)
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
data.reduced = data.relative_performance[, features.selection.player]

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
    return(cluster_analysis(data.relative_performance[, features.selection.player], kmax=120)$fits)
}, '../output/k-means-error-curve-player')

fits.team = render_plot(function () {
    return(cluster_analysis(team.normalized[, features.selection.team], kmax=120)$fits)
}, '../output/k-means-error-curve-team')

# render_plot(function () {
#     return(bagging_cluster_analysis(data.relative_performance[, features.selection.player], kmax=120, ntests=10))
# }, '../output/k-means-error-curve-player-avaraged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(data.relative_performance, kmax=120, ncol=ncol(data.relative_performance[, features.selection.player]), ntests=10))
# }, '../output/k-means-error-curve-player-bagging')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.normalized[, features.selection.team], kmax=120, ntests=10))
# }, '../output/k-means-error-curve-team-avareged')
#
# render_plot(function () {
#     return(bagging_cluster_analysis(team.normalized, kmax=120, ncol=ncol(team.normalized[, features.selection.team]), ntests=10))
# }, '../output/k-means-error-curve-team-bagging')

# Which is the optimal fit in this case? Analysing the error curve plot, the k = x fit seems to have the best trade-off,
# as the WSS rate for k > 8 does not vary so much after it.
fit = fits[[8]]

fit.team = fits.team[[6]]

# Labeling data
data = cbind(data, label=fit$cluster)
data.performance = cbind(data.performance, label=fit$cluster)
data.relative_performance = cbind(data.relative_performance, label=fit$cluster)

team.performance = cbind(team.performance, label=fit.team$cluster)
team.normalized = cbind(team.normalized, label=fit.team$cluster)

###############################################################################
# Clustered data balancing (undersampling) by discriminating winners and losers
###############################################################################

# Undersampling clustered data based on min size
balanced = balance(data.relative_performance, 'winner', 'label', 0.8)$data
# $size
#     all winners losers
# 1 35827   18317  17510
# 2 24702   11298  13404
# 3 15491    7213   8278
# 4 19528    6938  12590
# 5 23159   11902  11257
# 6 17871   11737   6134
# 7 17457    8991   8466
# 8 33415   17329  16086
#
# $relative_size
#     winners    losers
# 1 0.5112625 0.4887375
# 2 0.4573719 0.5426281
# 3 0.4656252 0.5343748
# 4 0.3552847 0.6447153
# 5 0.5139255 0.4860745
# 6 0.6567624 0.3432376
# 7 0.5150369 0.4849631
# 8 0.5185994 0.4814006
#
# $min_size
# [1] 4907

balanced.team = balance(team.normalized, 'winner', 'label')$data
# $size
#    all winners losers
# 1 4841    1234   3607
# 2 1853    1412    441
# 3 9268    4692   4576
# 4 8580    3127   5453
# 5 7875    4718   3157
# 6 5073    3562   1511
#
# $relative_size
#     winners    losers
# 1 0.2549060 0.7450940
# 2 0.7620076 0.2379924
# 3 0.5062581 0.4937419
# 4 0.3644522 0.6355478
# 5 0.5991111 0.4008889
# 6 0.7021486 0.2978514
#
# $min_size
# [1] 353

################################################################################################
# TODO Improve Statistical analysis of cluster analysis by discriminating a winner binary target
################################################################################################

cluster_statistical_analysis(data.relative_performance, features.selection.player, 'winner', 'label')

cluster_statistical_analysis(team.normalized, features.selection.team, 'winner', 'label')

##############################
# Cluster data exploring (Viz)
##############################

cluster_data_viz(balanced, features.selection.player, 'winner', 'label', pca_lim=c(-1, 1))

cluster_data_viz(balanced.team, features.selection.team, 'winner', 'label', pca_lim=c(-0.1, 0.1))

################################
# TODO Improve Centroid analysis
################################

centroid_analysis(balanced, features.selection.player, '../output/exploring-centers-player')

centroid_analysis(balanced.team, setdiff(features.selection.team, 'magicDamageDealtToMonsters'), '../output/exploring-centers-team')

#######################
# TODO Predictive model
#######################

import_package('caret', attach=TRUE)
import_package('logistf', attach=TRUE)

# Balance clustered 'label' data (undersampling) by discriminating 'winner' winners and losers
training_set = balance(team.performance, 'winner', 'label', prop=0.8)$data
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
# [1] 353

validation_set = team.performance[!(rownames(team.performance) %in% rownames(training_set)), ]

# > nrow(training_set)
# [1] 4236
# > nrow(validation_set)
# [1] 33254

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
# [1] 0.9558039
#
# $label2
# [1] 0.9555018
#
# $label3
# [1] 0.9509961
#
# $label4
# [1] 0.9160243
#
# $label5
# [1] 0.9288604
#
# $label6
# [1] 0.9476896

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
