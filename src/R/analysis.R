options(scipen=999)
options("width"=120)

library('modules')
import('utils', attach=TRUE)
import('fun', attach=TRUE)
import('outliers', attach=TRUE)
import('feature_selection', attach=TRUE)

RENDER_PLOT_SAVE = TRUE
RENDER_PLOT_CLOSE = TRUE

# =========================
# Specific domain functions
# =========================

#' Return matches with 10 players
get_players_from_5plus5_matches = function (players) {
    matchIds = names(Filter(Curry(eq, 10), table(players$matchId)))
    players = players[players$matchId %in% matchIds, ]
}

#' Return matches with 5 players each team (5x5). As data were looked up by participants, some matches were left with
#' less than 10 participants. So, these inconsistent (incomplete) matches need to be removed.
get_players_from_5x5_matches = function (data) {
    data = get_players_from_5plus5_matches(data)
    teams = aggregate(winner~matchId, data[, c('winner', 'matchId')], function(x) sum(as.numeric(x)))
    matchIds = teams[teams$winner == 5, 'matchId']
    return(data[data$matchId %in% matchIds, ])
}

#' Remove AFK players
remove_afk_players = function (data) {
    return(data[!(data$totalDamageDealtToChampions == 0 | data$goldSpent <= 500 | data$matchDuration < 20), ])
}

#' Remove low variance features from selected data features
unselect_low_variance_features = function (data, features) {
    low_variance_features = filter_features(data[, features], var, max=7)
    print(low_variance_features)
    return(setdiff(features, low_variance_features))
}

#' Remove redundant features from selected data features
unselect_redundant_features = function (data, features) {
    redundant_features = redundant_features(data[, features], threshold=0.65)
    print(redundant_features)
    return(setdiff(features, redundant_features))
}

# ======================================
# Load players' match statistics dataset
# ======================================

# A data set with n = 85470 points/tuples/rows, where a point p represents a feature vector of a participant in a
# specific match. Each match has only 10 participants.
data = read.csv('../data/data20170105025503.csv')

# nrow(data)
# [1] 1100000

# =============
# Data cleaning
# =============

# Remove some games with error or AFK players and inconsistent matches, i.e. matches not 5x5
data = get_players_from_5x5_matches(remove_afk_players(data))

# > nrow(data)
# [1] 1036090

# TODO Collect mode data to remove duplicated rows by summonerId and so as decrease bias
# data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)

# ==================
# Feature extraction
# ==================

features.target = 'winner'

# include inf, logical and categotial (nominal, ordinal or interval) features
features.info = c(
    'matchId',
    'matchMode',
    'queueType',
    'season',
    'matchDuration', # In minutes
    'matchCreation',
    'matchCreationYear',
    'matchCreationMonth',
    'matchCreationDay',
    'matchCreationHour',
    'summonerId',
    'championId',
    'champLevel',
    'item0',
    'item1',
    'item2',
    'item3',
    'item4',
    'item5',
    'item6',
    # 'combatPlayerScore',  # ARAM
    # 'objectivePlayerScore',  # ARAM
    # 'totalPlayerScore',  # ARAM
    # 'totalScoreRank',  # ARAM
    # 'unrealKills', # ARAM
    # 'firstBloodAssist',
    # 'firstBloodKill',
    'firstInhibitorAssist',
    'firstInhibitorKill'
    # 'firstTowerAssist',
    # 'firstTowerKill',
)

# Feature extraction / decomposition
# OBS: minionsKilledEnemyTeam not in minionsKilled
data[, 'physicalDamageDealtToMonsters'] = data[, 'physicalDamageDealt'] - data[, 'physicalDamageDealtToChampions']
data[, 'magicDamageDealtToMonsters'] = data[, 'magicDamageDealt'] - data[, 'magicDamageDealtToChampions']

features.numeric = sort(setdiff(names(data), c(features.target, features.info)))

# Remove compound feaures (leaving only atomic attributes - first normal form)
features.numeric = setdiff(features.numeric, c(
    'totalDamageDealt',  # physicalDamageDealt + magicDamageDealt
    'totalDamageDealtToChampions', # physicalDamageDealtToChampions + magicDamageDealtToChampions
    'totalDamageTaken', # physicalDamageTaken + magicDamageTaken
    'neutralMinionsKilled', # neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle
    'minionsKilled', #  neutralMinionsKilledEnemyJungle + neutralMinionsKilledTeamJungle + minionsKilledEnemyTeam
    'trueDamageDealt', # trueDamageDealtToChampions + trueDamageDealtToMonsters
    'physicalDamageDealt', # physicalDamageDealtToChampions + physicalDamageDealtToMonsters
    'magicDamageDealt' # magicDamageDealtToChampions + magicDamageDealtToMonsters
))

# ========================================
# Modeling teams' match statistics dataset
# ========================================
team = aggregate(. ~ matchId + winner, data=data[, c(features.numeric, 'winner', 'matchId')],  FUN=sum)

# > nrow(team)
# [1] 207218

# ================
# Data exploration
# ================

descriptive_statistics(data, features.numeric)
# > descriptive_statistics(data, features.numeric)
#                                  min    max     mean meadian           var       sd
# assists                            0     53     9.17       8         36.74     6.06
# deaths                             0     33     6.32       6         10.72     3.27
# doubleKills                        0     13     0.61       0          1.00     1.00
# goldEarned                      3122  43409 12230.98   11957   14853645.09  3854.04
# goldSpent                        535  63737 11158.36   10900   13816975.64  3717.12
# inhibitorKills                     0     10     0.19       0          0.23     0.48
# killingSprees                      0     12     1.45       1          1.77     1.33
# kills                              0     42     6.29       5         23.39     4.84
# largestCriticalStrike              0   3840   235.12       0     155227.06   393.99
# largestKillingSpree                0     29     2.81       2          6.58     2.57
# largestMultiKill                   0      6     1.40       1          0.57     0.76
# magicDamageDealtToChampions        0 128451  8970.28    5360   99515769.33  9975.76
# magicDamageTaken                   0  80065  9098.80    8046   30648713.40  5536.13
# neutralMinionsKilledEnemyJungle    0    132     4.02       1         50.56     7.11
# neutralMinionsKilledTeamJungle     0    178    14.79       4        515.29    22.70
# pentaKills                         0      3     0.00       0          0.00     0.05
# physicalDamageDealtToChampions     0 120687  9232.15    4732  105080249.50 10250.87
# physicalDamageTaken                0 112521 14895.72   13371   62825173.15  7926.23
# quadraKills                        0      5     0.01       0          0.01     0.12
# sightWardsBoughtInGame             0      0     0.00       0          0.00     0.00
# totalHeal                          0 110945  5024.79    3384   28222862.19  5312.52
# totalTimeCrowdControlDealt         0  26386   506.48     284     611388.45   781.91
# totalUnitsHealed                   0    142     2.49       1         18.68     4.32
# towerKills                         0     10     0.94       1          1.50     1.23
# tripleKills                        0      7     0.09       0          0.11     0.32
# trueDamageDealtToChampions         0 500151   987.35     428    5558953.87  2357.74
# trueDamageTaken                    0 501088  1016.52     648    4018263.08  2004.56
# visionWardsBoughtInGame            0     29     0.84       0          1.55     1.25
# wardsKilled                        0     81     2.14       1         10.24     3.20
# wardsPlaced                        0    595    12.36      11         56.88     7.54
# physicalDamageDealtToMonsters      0 913487 60927.52   35792 3900361598.99 62452.88
# magicDamageDealtToMonsters         0 691841 36682.91   19173 1959656258.61 44268.00

descriptive_statistics(team, features.numeric)
# > descriptive_statistics(team, features.numeric)
#                                   min     max      mean  meadian            var        sd
# assists                             0     166     45.84     44.0         508.31     22.55
# deaths                              0      96     31.58     32.0         159.58     12.63
# doubleKills                         0      16      3.05      3.0           4.95      2.23
# goldEarned                      20387  167863  61154.91  60916.0   292978978.54  17116.63
# goldSpent                       14245  173490  55791.81  55395.0   262537517.63  16203.01
# inhibitorKills                      0      14      0.97      0.0           1.47      1.21
# killingSprees                       0      27      7.27      7.0          11.93      3.45
# kills                               0      94     31.43     32.0         159.41     12.63
# largestCriticalStrike               0    6832   1175.60   1041.0      538117.54    733.56
# largestKillingSpree                 0      49     14.03     14.0          39.41      6.28
# largestMultiKill                    0      15      7.00      7.0           3.11      1.76
# magicDamageDealtToChampions      1454  277258  44851.41  40831.5   563467827.51  23737.48
# magicDamageTaken                 1455  280380  45494.02  41414.0   575104062.40  23981.33
# neutralMinionsKilledEnemyJungle     0     179     20.08     17.0         272.19     16.50
# neutralMinionsKilledTeamJungle      0     236     73.97     71.0         767.06     27.70
# pentaKills                          0       3      0.01      0.0           0.01      0.10
# physicalDamageDealtToChampions   1298  209321  46160.76  42946.5   482431013.08  21964.31
# physicalDamageTaken             11711  276723  74478.62  70797.5   732989853.03  27073.79
# quadraKills                         0       5      0.06      0.0           0.07      0.26
# sightWardsBoughtInGame              0       0      0.00      0.0           0.00      0.00
# totalHeal                        1756  178665  25123.93  22127.0   204083617.81  14285.78
# totalTimeCrowdControlDealt         40   44982   2532.42   1953.0     5085974.16   2255.21
# totalUnitsHealed                    2     149     12.46     10.0          79.87      8.94
# towerKills                          0      15      4.70      5.0          10.31      3.21
# tripleKills                         0       7      0.43      0.0           0.51      0.71
# trueDamageDealtToChampions          0  706412   4936.76   3597.0    37731918.11   6142.63
# trueDamageTaken                     0  717334   5082.62   3764.0    38076385.36   6170.61
# visionWardsBoughtInGame             0      43      4.18      4.0           9.02      3.00
# wardsKilled                         0     173     10.72      8.0         119.62     10.94
# wardsPlaced                         7     621     61.82     60.0         457.36     21.39
# physicalDamageDealtToMonsters   25356 1622946 304637.62 284105.5 19206507752.27 138587.55
# magicDamageDealtToMonsters       2664 1636239 183414.57 163108.5 11128259582.07 105490.57

# ========================
# Simple feature selection
# ========================

# Remove low variance features
features.selection.player = unselect_low_variance_features(data, features.numeric)
features.selection.team = unselect_low_variance_features(team, features.numeric)

# ================
# Outlier analysis
# ================

# Analyze and indentify extreme (IQR factor = 3) outliers of numeric features
outliers.player = render_plot(function () {
    return(outlier_analysis(data[, features.selection.player], factor=3))
}, '../output/outliers-player', width=16, height=12)

outliers.team = render_plot(function () {
    return(outlier_analysis(team[, features.selection.team], factor=3))
}, '../output/outliers-team', width=16, height=12)

# Remove matches with extreme outliers from team and player match stats data
extreme_outliers_mathIds = unique(c(data[outliers.player$outliers, 'matchId'], team[outliers.team$outliers, 'matchId']))
data = data[!data$matchId %in% extreme_outliers_mathIds, ]
team = team[!team$matchId %in% extreme_outliers_mathIds, ]

# Select only features where lower != upper from outlier analysis and remove low variance features
features.selection.player = unselect_low_variance_features(data, colnames(outliers.player$thresholds))
features.selection.team = unselect_low_variance_features(team, colnames(outliers.team$thresholds))

# > length(extreme_outliers_mathIds)
# [1] 84872

# > nrow(data)
# [1] 187370

# > nrow(team)
# [1] 37474

# ===================
# Data transformation
# ===================

# As the match duration varies between the matches, the features were divided by match duration to compute players
# performance per minute.
data.performance = as.data.frame(cbind(
    data[, c('matchId', 'winner')],
    data[, features.numeric]/data[, 'matchDuration']
))

# Team stats per minute
team.performance = aggregate(. ~ matchId + winner, data=data.performance,  FUN=sum)
team.performance = as.data.frame(sapply(team.performance, as.numeric))
rownames(team.performance) = rownames(team)

# =============
# Data Scalling
# =============

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
data.relative_performance = as.data.frame(sapply(data.relative_performance, as.numeric))
rownames(data.relative_performance) = rownames(data.performance)
data.relative_performance[is.na(data.relative_performance)] <- 0

# Since the team performances are of different varieties, their scales are also different. In order to maintain uniform
# scalability we normalize the features.
team.normalized = as.data.frame(cbind(
    normalize(team.performance[, features.numeric]),
    team.performance[, c('matchId', 'winner')]
))

# ===================================
# Correlation and similarity analysis
# ===================================
# Correlation matrix of normalized data using Spearman method, which does not require the features follow a normal
# distribuition or linear correlation.

render_plot(function () {
    return(correlation_analysis(data.relative_performance[, features.selection.player])$estimates)
}, '../output/correlation-player', width=18, height=12)

render_plot(function () {
    return(correlation_analysis(team.normalized[, features.selection.team])$estimates)
}, '../output/correlation-team', width=18, height=12)

# ========================================================
# Redundant feature selection analysis (correlation based)
# ========================================================

features.selection.player = unselect_redundant_features(data.relative_performance, features.selection.player)
features.selection.team = unselect_redundant_features(team.normalized, features.selection.team)

descriptive_statistics(data, features.selection.player)
# > descriptive_statistics(data, features.selection.player)
#                                 min    max     mean meadian           var       sd
# assists                           0     37     6.55     5.0         24.99     5.00
# deaths                            0     20     4.97     5.0          8.74     2.96
# kills                             0     27     4.94     4.0         17.40     4.17
# largestCriticalStrike             0   1736   176.25     0.0      90396.23   300.66
# magicDamageDealtToMonsters        0 203453 23584.20 12335.0  754433017.70 27466.94
# magicDamageTaken                  0  31830  6014.14  5328.0   13038175.01  3610.84
# neutralMinionsKilledEnemyJungle   0     20     2.57     1.0         17.31     4.16
# neutralMinionsKilledTeamJungle    0     68    11.10     2.0        323.20    17.98
# physicalDamageDealtToMonsters     0 328231 41910.22 27740.0 1541665215.44 39264.04
# physicalDamageTaken              49  47836 10835.40  9959.5   28931485.11  5378.80
# totalHeal                         0  20183  3067.04  2099.5    9089531.27  3014.88
# totalTimeCrowdControlDealt        0   2019   292.36   187.0      97850.26   312.81
# trueDamageDealtToChampions        0   4576   519.74   240.0     579366.59   761.16
# trueDamageTaken                   0   4325   541.72   382.0     308918.76   555.80
# wardsPlaced                       0     36     9.24     8.0         25.83     5.08

descriptive_statistics(team, features.selection.team)
# > descriptive_statistics(team, features.selection.team)
#                                   min    max      mean  meadian           var       sd
# killingSprees                       0     20      5.53      5.0          9.89     3.14
# largestCriticalStrike               0   4322    881.23    775.0     325100.80   570.18
# magicDamageDealtToMonsters       3492 489227 117921.01 108112.0 3581338203.01 59844.28
# magicDamageTaken                 1734 123100  30070.68  27272.5  229958245.14 15164.37
# neutralMinionsKilledEnemyJungle     0     59     12.84     11.0        107.95    10.39
# neutralMinionsKilledTeamJungle      0    145     55.48     55.0        286.30    16.92
# physicalDamageDealtToMonsters   28629 718083 209551.11 198025.5 7124174775.47 84404.83
# physicalDamageTaken             11711 160443  54177.00  51859.5  298468683.81 17276.25
# totalHeal                        1756  60022  15335.20  13997.0   53790618.64  7334.21
# totalTimeCrowdControlDealt         40   7639   1461.78   1300.0     549757.58   741.46
# trueDamageDealtToChampions          0  12353   2598.68   2180.0    3046197.94  1745.34
# trueDamageTaken                     0  13001   2708.60   2281.5    3303897.15  1817.66
# wardsKilled                         0     44      5.70      5.0         20.03     4.48
# wardsPlaced                         8    109     46.19     45.0        191.74    13.85

# Unselected features from numeric features
setdiff(features.numeric, features.selection.player)
# > setdiff(features.numeric, features.selection.player)
#  [1] "doubleKills"                    "goldEarned"                     "goldSpent"
#  [4] "inhibitorKills"                 "killingSprees"                  "largestKillingSpree"
#  [7] "largestMultiKill"               "magicDamageDealtToChampions"    "pentaKills"
# [10] "physicalDamageDealtToChampions" "quadraKills"                    "sightWardsBoughtInGame"
# [13] "totalUnitsHealed"               "towerKills"                     "tripleKills"
# [16] "visionWardsBoughtInGame"        "wardsKilled"

setdiff(features.numeric, features.selection.team)
# > setdiff(features.numeric, features.selection.team)
#  [1] "assists"                        "deaths"                         "doubleKills"
#  [4] "goldEarned"                     "goldSpent"                      "inhibitorKills"
#  [7] "kills"                          "largestKillingSpree"            "largestMultiKill"
# [10] "magicDamageDealtToChampions"    "pentaKills"                     "physicalDamageDealtToChampions"
# [13] "quadraKills"                    "sightWardsBoughtInGame"         "totalUnitsHealed"
# [16] "towerKills"                     "tripleKills"                    "visionWardsBoughtInGame"

# ========================
# Dimensionality reduction
# ========================

# Dimensionality reduction of the normalized data with selected features
data.reduced = data.relative_performance[, features.selection.player]

team.reduced = team.normalized[, features.selection.team]

render_plot(function () {
    return(correlation_analysis(data.reduced)$estimates)
}, '../output/correlation-player-thin', width=18, height=12)

render_plot(function () {
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
fit = fits[[7]]

fit.team = fits.team[[7]]

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
# 1 24915   12812  12103
# 2 17945   11839   6106
# 3 19601    9453  10148
# 4 18269    9417   8852
# 5 19501    6789  12712
# 6 33513   17334  16179
# 7 53626   26041  27585
#
# $relative_size
#     winners    losers
# 1 0.5142284 0.4857716
# 2 0.6597381 0.3402619
# 3 0.4822713 0.5177287
# 4 0.5154634 0.4845366
# 5 0.3481360 0.6518640
# 6 0.5172321 0.4827679
# 7 0.4856040 0.5143960
#
# $min_size
# [1] 4885

balanced.team = balance(team.normalized, 'winner', 'label')$data
# > balanced.team = balance(team.normalized, 'winner', 'label')$data
# $size
#    all winners losers
# 1 4849     364   4485
# 2 6432    3478   2954
# 3 3838    2637   1201
# 4 6274     319   5955
# 5 6648    3671   2977
# 6 5343    4723    620
# 7 4090    3545    545
#
# $relative_size
#      winners    losers
# 1 0.07506702 0.9249330
# 2 0.54073383 0.4592662
# 3 0.68707660 0.3129234
# 4 0.05084476 0.9491552
# 5 0.55219615 0.4478039
# 6 0.88396032 0.1160397
# 7 0.86674817 0.1332518
#
# $min_size
# [1] 255

################################################################################################
# TODO Improve Statistical analysis of cluster analysis by discriminating a winner binary target
################################################################################################

cluster_statistical_analysis(data.relative_performance, features.selection.player, 'winner', 'label')

cluster_statistical_analysis(team.normalized, features.selection.team, 'winner', 'label')

################################
# Cluster data exploration (Viz)
################################

cluster_data_viz(balanced, features.selection.player, 'winner', 'label', pca_lim=c(-1, 1))

cluster_data_viz(balanced.team, features.selection.team, 'winner', 'label', pca_lim=c(-0.01, 0.01))

centroid_analysis(balanced, features.selection.player, '../output/exploring-centers-player')

centroid_analysis(balanced.team, features.selection.team, '../output/exploring-centers-team')

# ==========================================================================
# TODO Modeling binary classfifier to predict a team victory / match outcome
# ==========================================================================

import_package('caret', attach=TRUE)
import_package('logistf', attach=TRUE)

# Balance clustered 'label' data (undersampling) by discriminating 'winner' winners and losers
training_set = balance(team.performance, 'winner', 'label', prop=0.8)$data
# > training_set = balance(team.performance, 'winner', 'label', prop=0.8)$data
# $size
#    all winners losers
# 1 4849     364   4485
# 2 6432    3478   2954
# 3 3838    2637   1201
# 4 6274     319   5955
# 5 6648    3671   2977
# 6 5343    4723    620
# 7 4090    3545    545
#
# $relative_size
#      winners    losers
# 1 0.07506702 0.9249330
# 2 0.54073383 0.4592662
# 3 0.68707660 0.3129234
# 4 0.05084476 0.9491552
# 5 0.55219615 0.4478039
# 6 0.88396032 0.1160397
# 7 0.86674817 0.1332518
#
# $min_size
# [1] 255

validation_set = team.performance[!(rownames(team.performance) %in% rownames(training_set)), ]

# > nrow(training_set)
# [1] 3570
# > nrow(validation_set)
# [1] 33904

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

    cluster_domain = sort(unique(training_set[, cluster_feature]))

    # train and validation result for each k cluster
    cluster_results = Map(function (k) {
        # filter k cluster data
        training_set = training_set[training_set[, cluster_feature] == k, c(features, target_feature)]
        validation_set = validation_set[validation_set[, cluster_feature] == k, c(features, target_feature)]

        # perform data reduction
        features = feature_engeneering(training_set, features, target_feature)
        training_set = training_set[, c(features, target_feature)]
        validation_set = validation_set[, c(features, target_feature)]

        # classfifier model
        model = train(as.formula(strf('%s ~ .', target_feature)), data=training_set, method="glm", family="binomial")

        # perform validation
        validation_result = validate(model, validation_set, features, target_feature)

        # cluster result
        return(list(k=k, features=features, model=model, validation_result=validation_result))
    }, cluster_domain)

    # labeling cluster result names
    names(cluster_results) = Map(function (k) strf('%s%s', cluster_feature, k), cluster_domain)

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

# TODO feature selection using known methods (intersection)
gini_index_selection = gini_index_selector(data, features.numeric, 'winner')
information_gain_selection = information_gain_selector(data, features.numeric, 'winner')
relieff_selection = relieff_selector(data, features.numeric, 'winner')
random_forest_selection = random_forest_selector(data, features.numeric, 'winner')

table(c(
    gini_index_selection$features,
    information_gain_selection$features,
    relieff_selection$features,
    random_forest_selection$features
))

# TODO remove features present in all clusters?
