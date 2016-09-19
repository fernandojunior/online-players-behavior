source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('../data/data20160609012259.csv')
# > nrow(data)
# [1] 85470

# Remove duplicated rows by summonerId to decrease bias
data = data[!duplicated(data[, 'summonerId']),]
# > nrow(data)
# [1] 54681

features = c(
    'assists', 'champLevel', 'deaths', 'doubleKills', 'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill', 'goldEarned',
    'goldSpent', 'inhibitorKills', 'killingSprees', 'kills', 'largestCriticalStrike',
    'largestKillingSpree', 'largestMultiKill', 'magicDamageDealt',
    'magicDamageDealtToChampions', 'magicDamageTaken', 'minionsKilled',
    'neutralMinionsKilled', 'neutralMinionsKilledEnemyJungle',
    'neutralMinionsKilledTeamJungle', 'pentaKills',
    'physicalDamageDealt', 'physicalDamageDealtToChampions',
    'physicalDamageTaken', 'quadraKills', 'sightWardsBoughtInGame',
    'totalDamageDealt', 'totalDamageDealtToChampions', 'totalDamageTaken',
    'totalHeal',
    'totalTimeCrowdControlDealt', 'totalUnitsHealed', 'towerKills',
    'tripleKills', 'trueDamageDealt', 'trueDamageDealtToChampions',
    'trueDamageTaken', 'visionWardsBoughtInGame', 'wardsKilled', 'wardsPlaced'
)

features.boolean = c(
    'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill'
)

features.numeric = setdiff(features, features.boolean)

# trash features: boolean, categorical, low variance
features.irrelevants = filter_features(data[, setdiff(features, features.boolean)], var, max=8)
# [1] "champLevel"              "doubleKills"
# [3] "inhibitorKills"          "killingSprees"
# [5] "largestKillingSpree"     "largestMultiKill"
# [7] "pentaKills"              "quadraKills"
# [9] "sightWardsBoughtInGame"  "towerKills"
# [11] "tripleKills"             "visionWardsBoughtInGame"
# [13] "wardsKilled"

features.irrelevants = filter_features(data[, setdiff(features, features.boolean)], var, max=8)

Filter(function (x) x < 8, Col(var, data[, setdiff(features, features.boolean)]))

features.irrelevants =  names(Filter(function (x) x < 8, Col(var, data[, features.numeric])))

Filter(function (x) x < 8, Col(var, data[, features.numeric]))

filter(data, var, function (x) x < 8)

Filter(function (x) x == TRUE, Col(function(x) var(x) < 8, data))

filter_features = function (x, f) {
    "Select the features of an data matrix x based on min > f(x) < max
    "
    y = apply(x, 2, f)
    # return(names(y[y == TRUE]))
    return(y)
}

filter = function (x, f) {
    "Select the features of an data matrix x based on min > f(x) < max
    "
    return(x[map(f, x) == TRUE])
}

filter_features(data[, setdiff(features, features.boolean)], function (feature) {
    return(var(feature) < 8)
})

Map(var, data[, 'assists'])

Col(var, data[, features.numeric])

x = Col(data[, features], var)

Filter(function (x) x < 8, Col(data[, features], var))

Filter(function (col) col < 8, Col(var, data[, setdiff(features, features.boolean)]))

pipe (map, filter(function (col) col > 8))

x = filter_features(data[, setdiff(features, features.boolean)], function (feature) {
    return((feature) < 8)
})


Reduce(data[, setdiff(features, features.boolean)], var)

apply(data[, setdiff(features, features.boolean)], 2, function (feature) {
    return(var(feature) < 8)
})

features.numeric = setdiff(features, c(features.boolean, features.irrelevants))
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

features.ong = c(
    'firstBloodKill', 'firstTowerKill', 'firstTowerAssist',
    'kills', 'assists', 'deaths', 'goldEarned','totalDamageDealt',
    'magicDamageDealt', 'physicalDamageDealt', 'totalDamageDealtToChampions',
    'totalDamageTaken', 'minionsKilled', 'neutralMinionsKilled',
    'totalTimeCrowdControlDealt', 'wardsPlaced', 'towerKills',
    'largestMultiKill', 'largestKillingSpree', 'largestCriticalStrike',
    'totalHeal'
)

features.ong2 = c(
    'goldEarned', 'kills', 'physicalDamageDealt', 'minionsKilled',
    'totalDamageTaken', 'towerKills', 'largestCriticalStrike',
    'neutralMinionsKilled', 'assists', 'totalTimeCrowdControlDealt',
    'magicDamageDealt', 'wardsPlaced', 'totalHeal', 'deaths'
)

features.mine = c(
    "totalDamageDealt", "goldEarned", "kills", "physicalDamageDealtToChampions",
    "totalDamageDealtToChampions", "killingSprees", "minionsKilled",
    "largestCriticalStrike", "neutralMinionsKilledEnemyJungle",
    "largestMultiKill", "trueDamageDealtToChampions",
    "neutralMinionsKilledTeamJungle", "towerKills", "totalDamageTaken",
    "magicDamageDealtToChampions", "wardsPlaced", "totalTimeCrowdControlDealt",
    "assists", "deaths", "trueDamageTaken", "totalHeal", "wardsKilled"
)

# Find extreme outliers
outliers = find_outliers(data[, features.numeric], factor=3)

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

# As the match duration varies between the matches, the features were divided
# by match duration. Since the features are of different varieties, their
# scales are also different. In order to maintain uniform scalability we
# normalize the integer features using Z-score. Boolean features do not need be
# normalized.
data.normalized = cbind(
    data[, features.boolean],
    scale(data[, features.irrelevants]/data[, 'matchDuration']),
    scale(data[, features.numeric]/data[, 'matchDuration'])
)

# Filtering entire data to remove extreme outliers
data.no.outliers = data[!outliers$outliers,]
data.normalized.no.outliers = data.normalized[!outliers$outliers,]
# > nrow(data.no.outliers)
# > nrow(data.normalized.no.outliers)
# [1] 43595

# --------------------
# Correlation analysis
# --------------------

correlation_analysis(data.normalized.no.outliers)
correlation_analysis(data.normalized.no.outliers[, features.ong])
correlation_analysis(data.normalized.no.outliers[, features.ong2])
correlation_analysis(data.normalized.no.outliers[, features.mine])
correlation_analysis(data.normalized.no.outliers[, features.numeric])

# ------------------------
# Dimensionality reduction: remove redundant features
# ------------------------

# The features with high similarity (dendogram plot) and high correlation
# (heatmap plot) > 0.7 are redundants. The following are redundant features.
# are similar? what is the most correlated |C(A)| > |C(B)| or |C(B)| > |C(A)|?
# abs(correlations$estimates[A, ]) > abs(correlations$estimates[B, ])
# length(x[x == TRUE])
features.unselect = c(
    'goldSpent',  # (goldEarned, goldSpent)
    'neutralMinionsKilled',  # (neutralMinionsKilled, neutralMinionsKilledTeamJungle)
    'kills',  # (kills, totalDamageDealtToChampions)
    'trueDamageTaken',  # (trueDamageTaken, magicDamageTaken)
    'physicalDamageDealt'  # (physicalDamageDealtToChampions, physicalDamageDealt)
)

features.selection = setdiff(features.selection, features.unselect)
# [1] "assists"                         "deaths"
# [3] "goldEarned"                      "largestCriticalStrike"
# [5] "magicDamageDealt"                "magicDamageDealtToChampions"
# [7] "magicDamageTaken"                "minionsKilled"
# [9] "neutralMinionsKilledEnemyJungle" "neutralMinionsKilledTeamJungle"
# [11] "physicalDamageDealtToChampions"  "physicalDamageTaken"
# [13] "totalDamageDealt"                "totalDamageDealtToChampions"
# [15] "totalDamageTaken"                "totalHeal"
# [17] "totalTimeCrowdControlDealt"      "trueDamageDealt"
# [19] "trueDamageDealtToChampions"      "wardsPlaced"

# ------------------------
# Cluster analysis / Learning model (K-means)
# ------------------------

#TODO https://en.wikipedia.org/wiki/Random_subspace_method
# https://www.quora.com/How-does-randomization-in-a-random-forest-work/answer/Mat-Kallada-1
#  bagging + RSS as we can create ensemble with strong diversity, reduce bias, and predictive power tends to increase drastically


par(mfrow=c(2, 6))
cluster_analysis(data.normalized)
cluster_analysis(data.normalized[, setdiff(features, features.boolean)])
cluster_analysis(data.normalized[, features.ong])
cluster_analysis(data.normalized[, setdiff(features.ong, features.boolean)])
cluster_analysis(data.normalized[, features.numeric])
cluster_analysis(data.normalized[, features.selection])

cluster_analysis(data.normalized.no.outliers)
cluster_analysis(data.normalized.no.outliers[, setdiff(features, features.boolean)])
cluster_analysis(data.normalized.no.outliers[, features.ong])
cluster_analysis(data.normalized.no.outliers[, setdiff(features.ong, features.boolean)])
cluster_analysis(data.normalized.no.outliers[, features.numeric])
cluster_analysis(data.normalized.no.outliers[, features.selection])

many_cluster_analysis(data.normalized.no.outliers[, setdiff(features, features.boolean)], ncol=20)
many_cluster_analysis(data.normalized[, setdiff(features, features.boolean)], ncol=20)

many_cluster_analysis(data.normalized.no.outliers[, features.numeric], ncol=10)
many_cluster_analysis(data.normalized[, features.numeric], ncol=10)

# como saber se as features escolhidas sao boas para agrupar?
# quanto mais features, mais detalhes sobre o comportamento dos jogadores
# quanto mais features, menos a variancia eh diminuida

par(mfrow=c(4, 5))
for (i in range(20)) {
    f = sample(features.numeric, sample(length(features.numeric), 1))
    print(i)
    cluster_analysis(data.normalized[, f], 10)
}

par(mfrow=c(4, 3))
for (i in range(5)) {
    f = sample(colnames(data.normalized.no.outliers), 20)
    print(i)
    cluster_analysis(data.normalized.no.outliers[, f], 20)
}

par(mfrow=c(4, 3))
for (i in range(5)) {
    f = sample(colnames(data.normalized.no.outliers), 20)
    print(i)
    cluster_analysis(data.normalized.no.outliers[, f], 20)
}


# [1] "trueDamageTaken"      "tripleKills"          "neutralMinionsKilled"
# [4] "kills"

# [1] "totalHeal"                  "inhibitorKills"
# [3] "champLevel"                 "assists"
# [5] "totalTimeCrowdControlDealt" "totalDamageDealt"
# [7] "visionWardsBoughtInGame"

# [1] "totalTimeCrowdControlDealt"      "neutralMinionsKilled"
# [3] "totalDamageTaken"                "pentaKills"
# [5] "magicDamageDealtToChampions"     "goldSpent"
# [7] "visionWardsBoughtInGame"         "trueDamageDealt"
# [9] "neutralMinionsKilledEnemyJungle" "minionsKilled"
# [11] "largestCriticalStrike"           "wardsPlaced"

par(mfrow=c(1, 4))
f = c("trueDamageTaken", "tripleKills", "neutralMinionsKilled", "kills")
cluster_analysis(data.normalized[, f], main='clusteziable')

f = c("trueDamageTaken", "neutralMinionsKilled",
"kills")
cluster_analysis(data.normalized, f, main='clusteziable')

f = c("trueDamageTaken", "minionsKilled", "kills")
cluster_analysis(data.normalized, f, main='clusteziable')

f = c("minionsKilled", "kills")
cluster_analysis(data.normalized, f, main='clusteziable')


# ver se n fatores escolhidos alteram..
n = sample(length(features.numeric), 1)
par(mfrow=c(4, 5))
for (i in range(20)) {
    f = sample(features.numeric, n)
    print(i)
    print(f)
    cluster_analysis(data.normalized[, f], i)
}

todos = c("totalTimeCrowdControlDealt", "physicalDamageTaken",  "towerKills", "minionsKilled",  "magicDamageDealt", "doubleKills",  "visionWardsBoughtInGame",  "quadraKills",  "kills",  "physicalDamageDealtToChampions",  "largestCriticalStrike",
 "deaths",  "totalDamageTaken",  "largestKillingSpree", "killingSprees",  "goldSpent", "totalDamageDealtToChampions",  "trueDamageDealt", "physicalDamageDealt",  "doubleKills", "magicDamageDealt",  "tripleKills",
 "magicDamageDealt", "goldSpent",  "doubleKills",  "neutralMinionsKilled",  "magicDamageTaken", "champLevel",  "totalDamageDealt", "totalTimeCrowdControlDealt",  "killingSprees",  "wardsPlaced",  "largestMultiKill",
 "pentaKills", "minionsKilled",  "totalHeal",  "trueDamageDealt",  "totalDamageDealtToChampions",  "magicDamageDealtToChampions",  "visionWardsBoughtInGame",  "inhibitorKills",  "physicalDamageDealtToChampions", "wardsKilled",  "largestKillingSpree",
 "deaths", "totalDamageDealtToChampions",  "physicalDamageDealtToChampions", "goldEarned",  "assists",  "champLevel",  "magicDamageDealt", "largestCriticalStrike",  "neutralMinionsKilledTeamJungle", "inhibitorKills",  "wardsKilled",
 "assists",  "kills",  "goldEarned", "totalTimeCrowdControlDealt",  "quadraKills",  "trueDamageTaken",  "physicalDamageDealtToChampions", "totalUnitsHealed",  "deaths",
 "physicalDamageDealt", "deaths",  "wardsPlaced", "totalDamageDealtToChampions",  "largestMultiKill",  "totalDamageTaken",  "killingSprees", "magicDamageDealtToChampions",  "neutralMinionsKilled",  "tripleKills",  "goldEarned",
 "totalDamageDealtToChampions",  "assists",  "inhibitorKills", "largestKillingSpree",  "trueDamageTaken",  "towerKills",  "trueDamageDealt",  "totalDamageTaken",  "pentaKills", "neutralMinionsKilledTeamJungle",  "magicDamageTaken",
 "trueDamageDealtToChampions",  "physicalDamageDealtToChampions",  "magicDamageTaken",  "totalDamageTaken",  "neutralMinionsKilledEnemyJungle", "pentaKills",  "champLevel",  "tripleKills",  "minionsKilled", "doubleKills",  "towerKills",
 "largestCriticalStrike", "quadraKills",  "totalHeal", "trueDamageTaken",  "minionsKilled", "kills",  "totalDamageTaken",  "magicDamageDealtToChampions",  "magicDamageDealt",  "totalDamageDealtToChampions",  "inhibitorKills",
 "totalDamageTaken", "totalHeal",  "neutralMinionsKilled", "physicalDamageDealtToChampions",  "neutralMinionsKilledTeamJungle", "inhibitorKills",  "magicDamageDealt", "towerKills",  "totalDamageDealtToChampions",  "deaths",  "totalUnitsHealed",
 "kills", "neutralMinionsKilledEnemyJungle",  "pentaKills",  "neutralMinionsKilled",  "trueDamageDealtToChampions",  "trueDamageTaken",  "largestKillingSpree", "magicDamageTaken",  "visionWardsBoughtInGame", "largestMultiKill",  "wardsKilled",
 "neutralMinionsKilledTeamJungle", "magicDamageTaken",  "totalDamageDealtToChampions",  "quadraKills",  "trueDamageTaken",  "wardsKilled",  "neutralMinionsKilled", "inhibitorKills",  "goldSpent",  "doubleKills",  "totalUnitsHealed",
 "towerKills",  "totalDamageDealtToChampions",  "tripleKills", "wardsKilled",  "trueDamageDealt", "neutralMinionsKilledEnemyJungle",  "trueDamageTaken", "doubleKills",  "pentaKills",  "totalUnitsHealed",  "champLevel",
 "towerKills", "magicDamageDealtToChampions",  "magicDamageDealt", "trueDamageDealtToChampions",  "visionWardsBoughtInGame",  "physicalDamageDealtToChampions",  "totalDamageDealtToChampions",  "inhibitorKills",  "trueDamageTaken",  "pentaKills",  "neutralMinionsKilled",
 "towerKills", "totalDamageDealt", "largestMultiKill", "trueDamageTaken",  "goldSpent",  "inhibitorKills", "totalHeal",  "killingSprees",  "pentaKills", "tripleKills",  "assists")

mean(sort(counter(todos)))

f = c("deaths",                           "magicDamageTaken",
"tripleKills",                     "wardsKilled",
"doubleKills",                     "neutralMinionsKilled",
"totalDamageTaken",                "magicDamageDealt",
"pentaKills",                      "physicalDamageDealtToChampions",
"towerKills",                      "inhibitorKills",
"trueDamageTaken",                 "totalDamageDealtToChampions")

cluster_analysis(data.normalized, f, main='clusteziable')

number_of_plots = 8

number_of_features = range(2, 5)

par(mfrow=c(length(number_of_features), number_of_plots))
n = 2
for (i in number_of_features) {
    for (j in range(number_of_plots)) {
        f = sample(features.numeric, n)
        print(j)
        print(f)
        cluster_analysis(data.normalized, f, j)
    }

}

random_cluster_analysis = function (data, ncol) {
    print(colnames(data))
    features = sample(colnames(data), ncol)
    # print(features)
    # cluster_analysis(data, features)
}

number_of_plots = 4
number_of_features = 11
par(mfrow=c(number_of_plots, number_of_plots))
for (i in range(number_of_plots)) {
    for (j in range(number_of_plots)) {
        print(i + j - 1)
        random_cluster_analysis(data.normalized[, features.mine], number_of_features)
    }
}

number_of_plots_x = 5
number_of_plots_y = 6
number_of_features = 1
x = data
par(mfrow=c(number_of_plots_x, number_of_plots_y))
for (i in range(number_of_plots_x * number_of_plots_y)) {
    if (i > length(features.numeric))
        break
    print(i)
    print(features.numeric[i])
    cluster_analysis(x[, features.numeric[i]], 12)
}

number_of_plots_x = 5
number_of_plots_y = 6
number_of_features = 1
par(mfrow=c(number_of_plots_x, number_of_plots_y))
for (i in range(number_of_plots_x * number_of_plots_y)) {
    if (i > length(features.numeric))
        break
    print(i)
    print(features.numeric[i])
    cluster_analysis(data.normalized, features.numeric[i], 'Error curve', 12)
}

number_of_plots_x = 5
number_of_plots_y = 6
number_of_features = 1
par(mfrow=c(number_of_plots_x, number_of_plots_y))
for (i in range(number_of_plots_x * number_of_plots_y)) {
    if (i > length(features.numeric))
        break
    print(i)
    print(features.numeric[i])
    cluster_analysis(data.no.outliers, features.numeric[i], 'Error curve', 12)
}

number_of_plots_x = 5
number_of_plots_y = 6
number_of_features = 1
par(mfrow=c(number_of_plots_x, number_of_plots_y))
for (i in range(number_of_plots_x * number_of_plots_y)) {
    if (i > length(features.numeric))
        break
    print(i)
    print(features.numeric[i])
    cluster_analysis(data.normalized.no.outliers, features.numeric[i], 'Error curve', 12)
}

par(mfrow=c(3, 4))
for (feature in features.irrelevants) {
    cluster_analysis(data, feature, 'Error curve', unique(data[, feature]))
}

# AQUI
ff = setdiff(colnames(features), c(features.boolean, features.irrelevants))

number_of_plots = 5
par(mfrow=c(4, number_of_plots))
for (number_of_features in range(3, 6)) {
    # number_of_features = 3
    x = data.normalized
    for (i in range(number_of_plots)) {
        f = sample(colnames(data.normalized), number_of_features)
        cluster_analysis(x[, f], kmax=10, main=i)
    }
}

par(mfrow=c(4, 5))
number_of_features = 9
x = data.normalized
for (i in range(20)) {
    f = sample(ff, number_of_features)
    cluster_analysis(x[, f], kmax=10)
}


many_cluster_analysis(data[, ff], nrand=1)


# K-means clustering model/fit for each k = {1, ..., nk} number of clusters
fits = t(map(
    function(k) kmeans(data[, ff], k, algorithm='Lloyd', iter.max=200),
    range(10)
))

# Total within-cluster SSE for each k-means clustering
twss = rowmap(function(fit) fit$tot.withinss, fits)
twss.prop = twss/twss[1]


# Plot to analyze the knee of error curve resultant of k-means clustering
ylab = 'tot.withinss(k)/tot.withinss(k=1)'
plot(twss.prop, main='', xlab='k', ylab=ylab, ylim=c(0, 1))



nfeatures = length(ff)
ntests = 3
nclusters = 10
x = data[, ff]

m = matrix(0, nrow=nfeatures, ncol=nclusters)

par(mfrow=c(4, 5))
for (i in range(nfeatures)) {
    m[i, ] = many_cluster_analysis(x, ncol=i, ntests=ntests)
}

par(mfrow=c(4, 7))
for (i in range(nrow(m))) {
    print(m[i, ])
    plot(m[i, ], main=i, xlab='k', ylab='twss[k]/twss[1]', ylim=c(0, 1))

}


par(mfrow=c(4, 5))
number_of_features = 9
x = data.normalized
for (i in range(20)) {
    f = sample(ff, number_of_features)
    cluster_analysis(x[, f], n=10)$totss.prop
}



n = 3
for (i in range(number_of_plots)) {
    f = sample(features.numeric, n)
    print(i)
    print(f)
    cluster_analysis(data.normalized, f, i)
}

n = 4
for (i in range(number_of_plots)) {
    f = sample(features.numeric, n)
    print(i)
    print(f)
    cluster_analysis(data.normalized, f, i)
}

n = 5
for (i in range(number_of_plots)) {
    f = sample(features.numeric, n)
    print(i)
    print(f)
    cluster_analysis(data.normalized, f, i)
}

# por feature
par(mfrow=c(4, 5))
for (i in range(20, 39)) {
    print(features.numeric[i])
    cluster_analysis(data.normalized, features.numeric[i], main=features.numeric[i])
}

# features clusterizaveis
# champLevel 2
# 'goldSpent' 2
# 'largestKillingSpree' 2
# 'largestMultiKill' 2
# 'totalTimeCrowdControlDealt' 2
# 'trueDamageDealt' 2
# 'trueDamageTaken' 2
# 'wardsPlaced' 5

features.clusterizable = c(
    'champLevel', 'goldSpent', 'largestKillingSpree', 'largestMultiKill',
    'totalTimeCrowdControlDealt', 'trueDamageDealt', 'trueDamageTaken',
    'wardsPlaced'
)

cluster_analysis(data.normalized, features.clusterizable, main='clusteziable')

#
features 1:length(features)
20 samples
