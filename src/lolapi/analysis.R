library("car")
library(cluster)  # for clusplot
library("scatterplot3d")  # scatterplot3d

options(scipen=999)

# ---------
# Constants
# ---------

PLOTS = 'plots/'

# --------------
# Some functions
# --------------

# helper functions

contains = function (l, e) {
    "Return TRUE if list l contains an element e, FALSE otherwise."
    return(e %in% l)
}

counter = function (...) {
    "Alias for table."
    return(table(...))
}

index = function (e, l) {
    "Alias for match."
    return(match(e, l))
}

len = function (...) {
    "Alias for length."
    return(length(...))
}

map = function (f, x) {
    "Apply a function f to a value x.

    If x is a vector or a list, it applies for each item in x.

    if x is a matrix, it applies for each item, for each column, in x.

    Examples:
        > map(function (a) a + 1, 1)
        [1] 2
        > map(function (a) a + 1, c(1,2,3))
        [1] 2 3 4
        > map(function (a) a + 1, rbind(c(1,2), c(1,2)))
             [,1] [,2]
        [1,]    2    3
        [2,]    2    3
    "
    if (is.list(x))
        lapply(x, f)
    else if (is.vector(x))
        sapply(x, f)
    else if (is.matrix(x))
        apply(x, 2, function (y) map(f, y))
}

range = function (...) {
    "Alias for seq.int."
    return(seq.int(...))
}

type = function (...) {
    "Alias for typeof."
    return(typeof(...))
}

values = function (x) {
    "Return only the values of an list or vector x."
    if (is.list(x))
        x = unlist(x)
    return(unname(x))
}

endswith = function (s, suffix) {
    "Return TRUE if s ends with the specified suffix, FALSE otherwise."
    return(grepl(format.string('%s$', suffix), s))
}

format.string = function (...) {
    "Alias for sprintf."
    return(sprintf(...))
}

startswith = function (s, prefix) {
    "Return TRUE if s starts with the specified prefix, FALSE otherwise."
    return(grepl(format.string('^%s', prefix), s))
}

# math functions

mss = function (x) {
    "Return the sum of square error of a multidimensional x sample data.

    Example:
        > x = cbind(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
        > ss(mvar(x), nrow(x), VAR=TRUE)  # sum of square error for each column
        [1] 10 10
        > mss(x)  # or simply
        [1] 10 10
    "
    return(apply(x, 2, ss))
}

mvar = function (x) {
    "Return the variance of a multidimensional x sample data.

    Example:
        > x = cbind(c(1,2,3,4,5), c(1,2,3,4,5))
        > mvar(x) # variance for each column
        [1] 2.5 2.5
    "
    return(apply(x,2,var))
}

ss = function (x, n=NA, VAR=FALSE) {
    "Return the sum of square error of a sample data x: (n - 1) * var(x).

    If VAR == TRUE, x is a [list of] variance of a sample data with n length.

    Example:
        > x = c(1, 2, 3, 4, 5)
        > length(x)
        [1] 5
        > var(x)
        [1] 2.5
        > ss(x)  # sum of square of x
        [1] 10
        > ss(2.5, 5, VAR=TRUE)  # pass only the variance and the size of x
        [1] 10
        > ss(c(2.5, 2.5), 5, VAR=TRUE)  # pass variances of a m. data n == 5
        [1] 10 10
    "
    if (VAR == FALSE) {
        if (is.vector(x))
            n = length(x)
        if (is.matrix(x))
            n = nrow(x)
        x = var(x)
    }
    return ((n - 1) * x)
}

# outlier functions

outlier_thresholds = function (x, factor=1.5) {
    "Find the lower and upper outlier thresholds of a specific data set x.

    Any point greater than upper threshold or less than lower threshold is
    considered an outlier.

    A factor is used to determine the upper and lower threshold. The default
    factor 1.5 (based on Turkey boxplot) indicates that the minimum and maximum
    ranges of a point is 50% less and greater than IQR, respectively.

    if x is a matrix or data frame, for each column in x.
    "
    if (is.matrix(x))
        return(t(do.call(rbind,
            apply(x, 2, function(y) outlier_thresholds(y, factor)
        ))))
    if (is.data.frame(x))
        return(t(do.call(rbind.data.frame,
            apply(x, 2, function(y) outlier_thresholds(y, factor)
        ))))

    quartiles = values(quantile(x)[2:4])
    first_quartile = quartiles[1]
    third_quartile = quartiles[3]
    iqr = third_quartile - first_quartile # same as IQR(x)

    threshold = list()
    threshold$lower = first_quartile - (iqr * factor)
    threshold$upper = (iqr * factor) + third_quartile
    return(threshold)
}

is.outlier = function (x, thresholds) {
    "Return TRUE if x is an outlier, FALSE otherwise.

    It's based on lower and upper thresholds.

    x point can be multivariate. In this case, the thresholds also must be."
    if(is.list(x) | is.vector(x))  # x is multivariate
        x = x[colnames(thresholds)]
    if (any(x < thresholds['lower', ]) | any(x > thresholds['upper', ]))
        return(TRUE)
    return(FALSE)
}

# correlation functions

cor.counter = function (x) {
    "Count the number of items from a list or vector of correlations x.

    If x is a matrix or data frame, count the number of items for each column.
    "
    if (is.matrix(x) | is.data.frame(x))
        return(apply(x, 2, cor.counter))

    return(counter(x))
}

cor.mean = function (x) {
    "Mean of a list or vector of correlations x.

    If x is a matrix or data frame, calculate the mean for each column.
    "
    if (is.matrix(x) | is.data.frame(x))
        return(apply(x, 2, cor.mean))
    return(mean(x[!x %in% NA]))
}

cor.rank = function (correlation_matrix, decreasing=TRUE) {
    "Correlation rank of the attributes of a correlation matrix.

    It is based on the mean of correlations for each one.
    "
    return(names(sort(cor.mmean(correlation_matrix), decreasing=decreasing)))
}

attribute_selection = function (correlation_matrix) {
    "Automatic attribute selection of a correlation matrix.

    It is based on the non-correlations counter and mean of correlations for
    each column (attribute) of the correlation matrix.

    Formalization:

        C = C(X) = {correlation matrix of a data X}

        A = A(X) = A(C) = {attributes of the correlation matrix C}

        c(a) in C = {correlations of attibute a}

        select(a) = 0, if qt0(a) > m(qt0(A)) And m(a) < md(m(A)); 1 otherwise
            where 0 = False and 1 = True

        selection(C) = { a in A(C) | select(a)}
    "
    # Correlation <= 5% is considered non-correlation
    # correlation_matrix = round(correlation_matrix, digits=1)  # lazy round
    correlation_matrix = map(  # hard round
        function(x) if (!is.na(x) & x <= 0.05) 0 else x,
        correlation_matrix
    )

    # A(C): Attributes of the correlation matrix C
    attrs = colnames(correlation_matrix)

    # qt(A): Correlations counter for each attribute A(C)
    qt = cor.counter(correlation_matrix)

    # qt0(A): Non-correlations counter for each attribute A(C)
    qt0 = map(function(x) values(qt[[x]]['0']), names(qt))
    qt0[is.na(qt0)] = 0  # replacing NA values

    # m(qt0(A)): Mean of qt0
    mqt0 = mean(values(qt0))

    # m(A): Mean of correlations for each a in A(C)
    m = cor.mean(correlation_matrix)

    # md(m(A)): Median of m(A)
    mdm = median(m)

    select =  function (attribute) {
        "Return TRUE if attribute must be selected, FALSE otherwise."
        if (!is.na(qt0[attribute]) & !is.na(m[attribute]))
            if (qt0[attribute] > mqt0 & m[attribute] < mdm)
                return(FALSE)
        return(TRUE)
    }

    # Applying select for each a in A(C) to indicate which must be selected
    filter = values(map(select, attrs))

    # Selecting (filtering) attributes
    selection = attrs[filter]

    return(selection)
}

# functions to save plots

save.png = function (f, ...) {
    "Save the output of a plot function f to a png file.

    Similar to savePlot(type='png').
    "
    name = 'Rplot'
    args = c(...)
    if (!is.null(args))
        if (!is.null(args['main']) & !is.na(args['main']))
            name = args['main']
    fname = as.character(substitute(f))
    filename = format.string('%s%s.%s.png', PLOTS, name, fname)
    png(file=filename)
    f(...)
    dev.off()
}

save.boxplot = function (...) {
    "Create a boxplot and save the output in a png file."
    save.png(boxplot, ...)
}

save.plot = function (...) {
    "Create a plot and save the output in a png file."
    save.png(plot, ...)
}

# deprecated functions

sum_of_squares = function (X) {
    "Return sum of squares of multidimensional X sample data: (n-1) * Var(X)."
    n = nrow(X) # size
    VarX = mvar(x)  # variance
    SS = (n-1) * VarX # sum of square
    result = c()
    result$size = n
    result$ss = SS
    result$var = VarX
    return(result)
}

total_sum_of_squares = function (X) {
    "Return total sum of squares of multidimensional X sample data: sum(ss(X))."
    ss = sum_of_squares(X)
    result = c()
    result$size = ss$size
    result$tss = sum(ss$ss)
    result$tvar = sum(ss$var)
    return(result)
}

# ---------
# Load data
# ---------

data = read.csv("data/ranked_matches_2015_ong_features.csv")
# > nrow(data)
# [1] 85470

# Data attributes
data.attrs = names(data)
data.attrs.info = data.attrs[1:4]
data.attrs.boolean = data.attrs[5:7]
data.attrs.integer = data.attrs[8:25]
data.attrs.numerical = c(data.attrs.boolean, data.attrs.integer)

# ---------------------
# Treatment of outliers
# ---------------------

# Boxplot to analyze the outliers of all integer attributes. Boolean attributes
# do not need be analyzed.
save.boxplot(
    data[, data.attrs.integer],
    main='[Outlier] Integer attributes',
    names=range(len(data.attrs.integer))
)

# As we can see from the above plot that some attributes has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
for (attr in data.attrs.integer) {
    title = format.string('[Outlier] %s', attr)
    save.plot(data[, attr], main=title)
    save.boxplot(data[, attr], main=title)
}

# Automatically finding the lower and upper extreme outlier thresholds
# (IQR factor = 3) for each integer attribute
thresholds = outlier_thresholds(data[, data.attrs.integer], factor=3)
# > t(thresholds)
#                                  lower    upper
# Kills                           -19.00     30.0
# Assists                         -19.00     37.0
# Deaths                          -11.00     24.0
# GoldEarned                    -7465.00  29957.0
# TotalDamageDealt            -229216.25 433109.8
# MagicDamageDealt            -112411.75 169991.0
# PhysicalDamageDealt         -260943.50 380499.8
# TotalDamageDealtToChampions  -33290.00  66089.0
# TotalDamageTaken             -25844.25  69949.0
# MinionsKilled                  -347.00    556.0
# NeutralMinionsKilled            -59.00     81.0
# CrowdControl                  -1023.00   1567.0
# WardsPlaced                     -17.00     32.0
# TowerKills                       -3.00      4.0
# LargestMultiKill                 -2.00      5.0
# LargestKillingSpree             -12.00     16.0
# LargestCritStrike             -1653.00   2204.0
# TotalHealAmount               -7896.00  11865.0

# Boolean vector to indicate which data point x is an extreme outlier or not.
outliers = apply(data, 1, function(x) is.outlier(x, thresholds))

# Filtering entire data to remove extreme outliers
data = data[!outliers,]
# > nrow(data)
# [1] 74656

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent matches need to be removed.

# Number of participants by match id
participants_by_match = counter(data$matchId)

# Matches (IDs) that do not contain all 10 participants
inconsistent_matches = names(participants_by_match[participants_by_match < 10])

# Removing inconsistent matches from data
data = data[!(data$matchId %in% inconsistent_matches),]
# > nrow(data)
# [1] 35140

# Plots to analyze the integer attributes after the treatments
save.boxplot(
    data[, data.attrs.integer],
    main='[Outlier] Integer attributes (after)',
    names=range(len(data.attrs.integer))
)

for (attr in data.attrs.integer) {
    title = format.string('[Outlier] %s (after)', attr)
    save.plot(data[, attr], main=title)
    save.boxplot(data[, attr], main=title)
}

# ----------------------------
# Data normalization (z-score)
# ----------------------------

# Since the data attributes are of different varieties their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer attributes using Z-score. Boolean attributes do not need be
# normalized.
data.normalized = cbind(
    data[, data.attrs.boolean],
    scale(data[, data.attrs.integer])
)

# --------------------
# Correlation analysis
# --------------------

# Correlation matrix of normalized data attributes
correlations = cor(data.normalized)
correlations = abs(correlations)  # the signal does not matter
diag(correlations) = NA  # correlation of a set with itself does not matter

# Boxplot to analyze attributes correlation
save.boxplot(
    correlations,
    main='[Correlation] Bolean and integer attributes',
    names=range(ncol(correlations))
)

# Attributes ranked by the mean of correlations for each one
data.attrs.ranked = cor.rank(correlations)
# [1] "GoldEarned"                  "TotalDamageDealt"
# [3] "TotalDamageDealtToChampions" "Kills"
# [5] "PhysicalDamageDealt"         "MinionsKilled"
# [7] "LargestKillingSpree"         "LargestMultiKill"
# [9] "LargestCritStrike"           "TowerKills"
# [11] "TotalDamageTaken"            "Assists"
# [13] "MagicDamageDealt"            "CrowdControl"
# [15] "WardsPlaced"                 "NeutralMinionsKilled"
# [17] "Deaths"                      "FirstTower"
# [19] "TotalHealAmount"             "FirstBlood"
# [21] "FirstTowerAssist"

# ------------------------
# Dimensionality reduction
# ------------------------

# Automatic attribute selection based on the correlation matrix
data.attrs.selection = attribute_selection(correlations)
# [1] "Kills"                       "GoldEarned"
# [3] "TotalDamageDealt"            "PhysicalDamageDealt"
# [5] "TotalDamageDealtToChampions" "TotalDamageTaken"
# [7] "MinionsKilled"               "WardsPlaced"
# [9] "TowerKills"                  "LargestMultiKill"
# [11] "LargestKillingSpree"         "LargestCritStrike"

# Selection with attributes ranked
data.attrs.rankedselection = intersect(data.attrs.ranked, data.attrs.selection)

# Top 3 most correled attributes of the selection
data.attrs.topselection = data.attrs.rankedselection[1:3]

# Reducing the dimensionality of the normalized data with selected attributes
data.reduced = data.normalized[, data.attrs.selection]

# ----------------
# K-means analysis
# ----------------

# To find the optimal k number of clusters we can use the method that finds the
# knee of the error curve, which tries to find an appropriate number of
# clusters analyzing the curve of a generated graph from a test (based on
# k-means) conducted for each possible.

# Total within sum of squares of clusters for each k-means test k <= 20
twss = map(
    function(k) kmeans(data.reduced, k, algorithm='Lloyd')$tot.withinss,
    range(50)
)

# Plot to analyze the knee of error curve resultant of the test for each k
save.plot(twss, main="[K-means] Error curve", xlab="k", ylab="tot.withinss")

# Which is the optimal k value in this case? k={4, 5, 6, 7, 8, 9}?

fit0 = kmeans(data.reduced, 4, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  48.3 %)

fit1 = kmeans(data.reduced, 5, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  53.5 %)
# fit1 - fit0 = 5.2 %

fit2 = kmeans(data.reduced, 6, algorithm='Lloyd', iter.max=200)
# (between_SS / total_SS =  56.7 %)
# fit2 - fit1 = 3.2 %

fit3 = kmeans(data.reduced, 7, algorithm='Lloyd')
# (between_SS / total_SS =  59.1 %)
# fit3 - fit2 = 2.4 %

fit4 = kmeans(data.reduced, 8, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  60.7 %)
# fit4 - fit3 = 1.6 %

fit5 = kmeans(data.reduced, 9, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  62.2 %)
# fit5 - fit4 = 1.5 %

# fit4 has the best trade-off:
plot(c(4:8), c(5.2, 3.2, 2.4, 1.6, 1.5), xlab='fit', ylab='fit[i] - fit[i-1]')

# Let's add some extra components to fit4, then save.

# Variance for each cluster of fit4
fit4$withinvar = 1 / (fit4$size - 1) * fit4$withinss

# Number of clusters of fit4
fit4$k = len(fit4$size)

# Saving all components of fit4
for (component in names(fit4))
    write.csv(fit4[[component]], format.string('data/fit4/%s.csv', component))

# --------------------------------------------------
# Analysis of the data labeled by k-means clustering
# --------------------------------------------------

# Associating each reduced tuple with its info, win and label attributes
data.labeled = cbind(data[, data.attrs.info], label=fit4$cluster, data.reduced)

# Spliting labeled data between winners and losers
vencedores = data.labeled[data.labeled$Win == 1,]
perdedores = data.labeled[data.labeled$Win == 0,]

# Clusplot analysis
# -----------------

# A sample with n=80 random rows from labeled data
data.sampled = data.labeled[sample(range(nrow(data.labeled)), 80),]

# Clusplot of clusterized data (n=80)
clusplot(
    data.sampled[, data.attrs.selection],
    data.sampled$label,
    labels=4,
    col.clus= sort(unique(data.sampled$label)),
    col.p=data.sampled$label,
    lines=0
)

# Scatter plot analysis
# ---------------------

# Plot of the labeled data
plot(data.labeled[, data.attrs.selection], col=data.labeled$label)

# Only top correlated attributes
plot(data.labeled[, data.attrs.topselection], col=data.labeled$label)

# Scatterplot of most correlated attributes for each split
plot(vencedores[, data.attrs.topselection], col=vencedores$label)
plot(perdedores[, data.attrs.topselection], col=perdedores$label)

# Principal Component Analysis (PCA)
------------------------------------

# PCA of labeled data
data.labeled.pca = prcomp(data.labeled[, data.attrs.selection], center=TRUE)

# PCA of winners
vencedores.pca = prcomp(vencedores[, data.attrs.selection], center=TRUE)

# PCA of losers
perdedores.pca = prcomp(perdedores[, data.attrs.selection], center=TRUE)

# Principal components to view
pca_indices = c(1, 2, 3)

# 3-D visualization of principal components of the labeled data
scatterplot3d(
    data.labeled.pca$x[, pca_indices],
    pch=data.labeled$label,
    type="h",
    angle=95,  # 30
    color=data.labeled$label
)

# 3-D visualization of principal components of winners
scatterplot3d(
    vencedores.pca$x[, pca_indices],
    pch=vencedores$label,
    type="h",
    angle=95,
    color=vencedores$label
)

# 3-D visualization of principal components of losers
scatterplot3d(
    perdedores.pca$x[, pca_indices],
    pch=perdedores$label,
    type="h",
    angle=95,
    color=perdedores$label
)

# In general, we can clearly observe the k clusters found in k-means clustering.
# We can also observe that some clusters are more perceptible than others when
# the labeled data is splited between winners and losers.

# TODO Summarying partitions
----------------------------

# Within cluster sum of squares
withinss = function (data, cluster) {
    X = data[data$Cluster == cluster, ]
    tss = total_sum_of_squares(X)
    return(c(tss$size, tss$tss, tss$tvar))
}

vencedores = c('size', 'withinss', 'var')
perdedores = c('size', 'withinss', 'var')

for(i in c(1:8)) {
    vencedores = rbind(i=vencedores, withinss(vencedores, i))
    perdedores = rbind(i=perdedores, withinss(perdedores, i))
}

# winners output TODO put in csv file
# 3739 41316.9371603666 11.0532202141163
# 4281 19268.3432556076 4.50194935878682
# 5223 49015.2318475736 9.38629487697695
# 5179 15236.4693924323 2.94253947323915
# 4035 27864.5755564661 6.90743072792912
# 3691 18353.4704144465 4.97384022071721
# 5835 46343.2471402998 7.94364880704487
# 9667 52208.3269817618 5.40123391079679

# losers output TODO put in csv file
# 1674 16687.9878413122 9.97488812989375
# 3772 16583.1388273172 4.39754410695233
# 3217 25773.5785903237 8.01417244723994
# 11838 34719.6991407263 2.9331502188668
# 6453 39535.8716607882 6.12769244587541
# 10711 51691.7332768328 4.82649236945218
# 418 2547.04791512543 6.10802857344227
# 3567 14730.9541129319 4.13094618982947

# TODO centers of each partition ... scaled and not scaled

# mean of all attributes (not scaled) grouped by clusters
tmp = data[, data.attrs.selection]
centers_not_scaled = aggregate(. ~ Cluster, tmp, mean)
write.csv(centers_not_scaled[, c(2:ncol(centers_not_scaled))], file = "analysis/cluster/testes/fit4/centers_not_scaled.csv")

# TODO
# cluster, participants, winners, losers, winrate (w/(w+l))
# 1 5413 3739 1674 0.6907445039719194
# 2 8053 4281 3772 0.5316031292685955
# 3 8440 5223 3217 0.6188388625592417
# 4 17017 5179 11838 0.304342716107422
# 5 10488 4035 6453 0.3847254004576659
# 6 14402 3691 10711 0.25628384946535204
# 7 6253 5835 418 0.9331520869982408
# 8 13234 9667 3567 0.7304669789935015

# ----------
# Hypothesis
# ----------

# H1-0: Não existe diferença entre as distribuições dos clusters encontrados no
# modelo de aprendizagem

# dados normalizados z-score
kruskal.test(rowSums(data.reduced), fit4$cluster)
# Kruskal-Wallis rank sum test
# Kruskal-Wallis chi-squared = 30223, df = 7, p-value < 2.2e-16

# analisando o tamanho de cada cluster: não existe diferença?
wilcox.test(fit4$size, conf.int=T)
# Wilcoxon signed rank test
# V = 36, p-value = 0.007813
# alternative hypothesis: true location is not equal to 0
# 95 percent confidence interval:
# 49032.26 73588.02
# sample estimates:
# (pseudo)median
#      62429.24

# H2-0: Para cada cluster encontrado, não existe diferença entre as medianas
# dos jogadores vitoriosos e perdedores

x = rowSums(vencedores[ vencedores$label == 1, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 1, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 3452800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 2, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 2, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 399230, p-value = 0.02427
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 3, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 3, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 6572200, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 4, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 4, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1983800, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 5, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 5, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 5152600, p-value = 5.074e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 6, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 6, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 1006300, p-value = 1.915e-10
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 7, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 7, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 828060, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$label == 8, data.attrs.selection])
y = rowSums(perdedores[ perdedores$label == 8, data.attrs.selection])
wilcox.test(x , y, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# W = 2654500, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# References
# [Kardi Teknomo] Kardi Teknomo. ANALYTIC HIERARCHY PROCESS (AHP) TUTORIAL. https://docs.google.com/file/d/0BxYU82vErc8xYS1PendBeHlpSkk/edit?usp=sharing
# http://stats.stackexchange.com/questions/49521/how-to-find-variance-between-multidimensional-points
# http://www.edureka.co/blog/clustering-on-bank-data-using-r/
# http://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
# http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
# http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
# http://blog.melchua.com/2012/09/07/r-basics-accessing-elements-of-an-object/
# https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusplot.default.html
