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
    return(data[!(data$totalDamageDealtToChampions == 0 | data$matchDuration < 20 | data$goldSpent <= 3000), ])
}

#' Remove low variance features from selected raw data features
unselect_low_variance_features = function (raw_data, features) {
    low_variance_features = filter_features(raw_data[, features], function(col) round(var(col)), max=7)
    print(low_variance_features)
    return(setdiff(features, low_variance_features))
}

#' Remove redundant features from selected data features
unselect_redundant_features = function (data, features) {
    redundant_features = redundant_features(data[, features], correlation_threshold=0.65)
    print(redundant_features)
    return(setdiff(features, redundant_features))
}

#' Analyze and indentify matches with outliers (IQR factor = x) from player and team data
get_outlier_match_ids = function (players, teams, IQR_factor=1.5) {
    are_outlier_players = render_plot(function () {
        return(outlier_analysis(players[, setdiff(colnames(players), 'matchId')], factor=IQR_factor)$outliers)
    }, '../output/outliers-player', width=16, height=12)

    are_outlier_teams = render_plot(function () {
        return(outlier_analysis(teams[, setdiff(colnames(teams), 'matchId')], factor=IQR_factor)$outliers)
    }, '../output/outliers-team', width=16, height=12)

    return(unique(c(players[are_outlier_players, 'matchId'], teams[are_outlier_teams, 'matchId'])))
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
# [1] 1009770

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
    'combatPlayerScore',  # ARAM
    'objectivePlayerScore',  # ARAM
    'totalPlayerScore',  # ARAM
    'totalScoreRank',  # ARAM
    'unrealKills', # ARAM
    'firstBloodAssist',
    'firstBloodKill',
    'firstInhibitorAssist',
    'firstInhibitorKill',
    'firstTowerAssist',
    'firstTowerKill'
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
    'trueDamageDealt', # trueDamageDealtToChampions + trueDamageDealtToMonsters
    'physicalDamageDealt', # physicalDamageDealtToChampions + physicalDamageDealtToMonsters
    'magicDamageDealt' # magicDamageDealtToChampions + magicDamageDealtToMonsters
))

# ========================================
# Modeling teams' match statistics dataset
# ========================================
team = aggregate(. ~ matchId + winner, data=data[, c(features.numeric, 'winner', 'matchId')],  FUN=sum)

# > nrow(team)
# [1] 201954

# ================
# Data exploration
# ================

descriptive_statistics(data, features.numeric)
# > descriptive_statistics(data, features.numeric)
#                                  min    max     mean meadian           var       sd
# assists                            0     53     9.28       8         36.65     6.05
# deaths                             0     33     6.37       6         10.63     3.26
# doubleKills                        0     13     0.62       0          1.01     1.00
# goldEarned                      3381  43409 12324.99   12044   14581987.34  3818.64
# goldSpent                       3005  63737 11255.80   10975   13468195.75  3669.90
# inhibitorKills                     0     10     0.19       0          0.23     0.48
# killingSprees                      0     12     1.47       1          1.77     1.33
# kills                              0     42     6.34       5         23.46     4.84
# largestCriticalStrike              0   3840   237.16       0     156860.70   396.06
# largestKillingSpree                0     29     2.82       2          6.57     2.56
# largestMultiKill                   0      6     1.41       1          0.57     0.76
# magicDamageDealtToChampions        0 128451  9074.89    5457  100549138.71 10027.42
# magicDamageDealtToMonsters         0 691841 37066.88   19499 1979653360.87 44493.30
# magicDamageTaken                   0  80065  9205.25    8150   30520693.83  5524.55
# minionsKilled                      0    753   124.01     124       6924.06    83.21
# neutralMinionsKilledEnemyJungle    0    132     4.04       1         50.79     7.13
# neutralMinionsKilledTeamJungle     0    178    14.92       4        519.78    22.80
# pentaKills                         0      3     0.00       0          0.00     0.05
# physicalDamageDealtToChampions     0 120687  9331.40    4834  106248368.02 10307.68
# physicalDamageDealtToMonsters      0 913487 61493.67   36433 3939565664.99 62765.96
# physicalDamageTaken                0 112521 15031.73   13507   62688066.54  7917.58
# quadraKills                        0      5     0.01       0          0.01     0.12
# sightWardsBoughtInGame             0      0     0.00       0          0.00     0.00
# totalHeal                          0 110945  5080.25    3427   28489269.56  5337.53
# totalTimeCrowdControlDealt         0  26386   511.24     288     617193.22   785.62
# totalUnitsHealed                   0    142     2.50       1         18.86     4.34
# towerKills                         0     10     0.95       1          1.51     1.23
# tripleKills                        0      7     0.09       0          0.11     0.33
# trueDamageDealtToChampions         0 500151   999.10     440    5619292.14  2370.50
# trueDamageTaken                    0 501088  1028.38     658    4052023.63  2012.96
# visionWardsBoughtInGame            0     29     0.84       0          1.57     1.25
# wardsKilled                        0     81     2.17       1         10.34     3.22
# wardsPlaced                        0    595    12.48      11         56.95     7.55

descriptive_statistics(team, features.numeric)
# > descriptive_statistics(team, features.numeric)
#                                   min     max      mean  meadian            var        sd
# assists                             0     166     46.41     45.0         501.83     22.40
# deaths                              0      96     31.86     32.0         157.25     12.54
# doubleKills                         0      16      3.09      3.0           4.95      2.23
# goldEarned                      21241  167863  61624.94  61313.0   285885117.17  16908.14
# goldSpent                       17525  173490  56279.00  55783.0   254935731.04  15966.71
# inhibitorKills                      0      14      0.97      0.0           1.48      1.22
# killingSprees                       0      27      7.34      7.0          11.81      3.44
# kills                               0      94     31.71     32.0         157.08     12.53
# largestCriticalStrike               0    6832   1185.81   1051.0      538830.36    734.05
# largestKillingSpree                 0      49     14.11     14.0          38.85      6.23
# largestMultiKill                    0      15      7.04      7.0           3.05      1.75
# magicDamageDealtToChampions      1454  277258  45374.46  41341.0   559898084.93  23662.17
# magicDamageDealtToMonsters       2664 1636239 185334.42 165063.0 11104812464.71 105379.37
# magicDamageTaken                 1455  280380  46026.27  41934.0   571260353.46  23901.05
# minionsKilled                     142    1988    620.06    606.0       29672.00    172.26
# neutralMinionsKilledEnemyJungle     0     179     20.20     17.0         272.66     16.51
# neutralMinionsKilledTeamJungle      0     236     74.58     72.0         759.39     27.56
# pentaKills                          0       3      0.01      0.0           0.01      0.11
# physicalDamageDealtToChampions   2516  209321  46657.02  43445.0   478524140.52  21875.19
# physicalDamageDealtToMonsters   25356 1622946 307468.35 286914.5 19065412611.47 138077.56
# physicalDamageTaken             12963  276723  75158.66  71446.5   722512844.62  26879.60
# quadraKills                         0       5      0.06      0.0           0.07      0.26
# sightWardsBoughtInGame              0       0      0.00      0.0           0.00      0.00
# totalHeal                        1756  178665  25401.26  22394.0   203814822.76  14276.37
# totalTimeCrowdControlDealt         40   44982   2556.19   1974.0     5118253.86   2262.36
# totalUnitsHealed                    3     149     12.52     10.0          80.57      8.98
# towerKills                          0      15      4.73      5.0          10.25      3.20
# tripleKills                         0       7      0.44      0.0           0.51      0.72
# trueDamageDealtToChampions          0  706412   4995.48   3654.0    38146615.15   6176.29
# trueDamageTaken                     0  717334   5141.91   3822.0    38482885.65   6203.46
# visionWardsBoughtInGame             0      43      4.21      4.0           9.08      3.01
# wardsKilled                         0     173     10.87      8.0         120.48     10.98
# wardsPlaced                         7     621     62.39     60.0         449.11     21.19

# ========================
# Simple feature selection
# ========================

# Remove low variance features
features.selection.player = unselect_low_variance_features(data, features.numeric)
features.selection.team = unselect_low_variance_features(team, features.numeric)

# ================
# Outlier analysis
# ================

# Analyze and indentify matches with extreme outliers
outlier_match_ids = get_outlier_match_ids(
    data[, c(features.selection.player, 'matchId')],
    team[, c(features.selection.team, 'matchId')],
    IQR_factor=3.5
)

# Remove matches with extreme outliers from team and player match stats data
data = data[!data$matchId %in% outlier_match_ids, ]
team = team[!team$matchId %in% outlier_match_ids, ]

# Remove low variance features after outliers clean
features.selection.player = unselect_low_variance_features(data, features.selection.player)
features.selection.team = unselect_low_variance_features(team, features.selection.team)

# > length(outlier_match_ids)
# [1] 75290

# > nrow(data)
# [1] 256870

# > nrow(team)
# [1] 51374

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

RENDER_PLOT_SAVE = NULL
RENDER_PLOT_CLOSE = NULL

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

testing_set = team.performance[!(rownames(team.performance) %in% rownames(training_set)), ]

# > nrow(training_set)
# [1] 3570
# > nrow(testing_set)
# [1] 33904

#' Test a predictive model given a testing set
test_model = function (model, testing_set, features, target_feature) {
    # target classes can't be numeric
    targets = as.factor(testing_set[, target_feature])

    outcomes_prob_by_target = predict(model, newdata=testing_set[, features, drop=FALSE], type="prob")

    outcomes = sapply(1:nrow(testing_set), function (i) {
        current_target = targets[i]
        outcome_prob = outcomes_prob_by_target[i, current_target]
        if (outcome_prob > 0.5 & current_target == 0 || outcome_prob < 0.5 & current_target == 1)
            return(0)
        return(1)
    })

    confusion_matrix = table(outcomes, targets)
    accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
    # TODO precision and recall
    return(list(confusion_matrix=confusion_matrix, accuracy=accuracy))
}

train_model = function (training_set, testing_set, features, target_feature) {
    training_set = training_set[, c(features, target_feature)]
    testing_set = testing_set[, c(features, target_feature)]

    # perform data reduction
    features = feature_engeneering(training_set, features, target_feature, correlation_threshold=0.55)
    training_set = training_set[, c(features, target_feature)]
    testing_set = testing_set[, c(features, target_feature)]

    # classfifier model
    model = train(as.formula(strf('%s ~ .', target_feature)), data=training_set, method="glm", family="binomial")

    # test performance
    performance = test_model(model, testing_set, features, target_feature)

    # result
    return(list(features=features, model=model, performance=performance))
}

# partitions = caret::createDataPartition(training[, target_feature], p=0.6, list=FALSE)
# training_set = as.data.frame(training[partitions, ])
# testing_set = as.data.frame(training[-partitions, ])

#' Train and test binary prediction model for each cluster
train_model_by = function (training_set, testing_set, features, target_feature, cluster_feature) {
    training_set = training_set[, c(features, target_feature, cluster_feature)]
    training_set = if (!is.data.frame(training_set)) as.data.frame(training_set) else training_set

    # target classes can't be numeric in Predictive modeling
    training_set[, target_feature] = as.factor(training_set[, target_feature])

    cluster_classes = sort(unique(training_set[, cluster_feature]))

    # train and validation result for each k cluster
    cluster_results = Map(function (k) {
        # filter k cluster data
        training_set = training_set[training_set[, cluster_feature] == k, c(features, target_feature)]
        testing_set = testing_set[testing_set[, cluster_feature] == k, c(features, target_feature)]

        learning_result = train_model(training_set, testing_set, features, target_feature)
        render_plot(function () correlation_analysis(training_set[, learning_result$features]))
        learning_result$k = k
        return(learning_result)
    }, cluster_domain)

    # labeling cluster result names
    names(cluster_results) = Map(function (k) strf('%s%s', cluster_feature, k), cluster_classes)

    return(cluster_results)
}

learning_result = train_model_by(training_set, testing_set, features.selection.team,  'winner', 'label')
Map(function(k) k$performance$accuracy, learning_result)
# $label1
# [1] 0.9525097
#
# $label2
# [1] 0.9187447
#
# $label3
# [1] 0.9025564
#
# $label4
# [1] 0.8900849
#
# $label5
# [1] 0.9374898
#
# $label6
# [1] 0.9283245
#
# $label7
# [1] 0.9153953

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
