library("car")
library(cluster)  # for clusplot
library("scatterplot3d")  # scatterplot3d

options(scipen=999)

# Constants
# ---------

PLOTS = 'plots/'

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

# correlation functions

cor.counter = function (correlations, select=NULL) {
    "Count the number of items from a list (or vector) of correlations.

    Specific item(s) name can be selected.
    "
    if (!is.null(select))
        return(counter(correlations)[select])
    return(counter(correlations))
}

cor.mcounter = function (mcorrelations, select=NULL) {
    "Count the number of items for each column of a matrix of correlations.

    Specific item(s) name can be selected.
    "
    return(apply(mcorrelations, 2, function(col) cor.counter(col, select)))
}

cor.mean = function (correlations) {
    "Mean of a list or vector of correlations."
    return(mean(correlations[!correlations %in% NA]))
}

cor.mmean = function (mcorrelations) {
    "Mean for each column of a matrix of correlations."
    return(apply(mcorrelations, 2, cor.mean))
}

cor.rank = function (correlation_matrix, decreasing=TRUE) {
    "Correlation rank of the attributes of a correlation matrix.

    It is based on the mean of correlations for each one.
    "
    return(names(sort(cor.mmean(correlations), decreasing=decreasing)))
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
    attributes = colnames(correlation_matrix)

    # qt0(A): Non-correlations counter for each attribute A(C)
    qt0 = cor.mcounter(correlation_matrix, '0')

    # m(qt0(A)): Mean of qt0
    mqt0 = mean(values(qt0))

    # m(A): Mean of correlations for each a in A(C)
    m = cor.mmean(correlation_matrix)

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
    filter = values(map(select, attributes))

    # Selecting (filtering) attributes
    selection = attributes[filter]

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

# Load data
# ---------

data = read.csv("data/ranked_matches_2015_ong_features.csv")

# Data attributes
attributes = names(data)
attributes.info = attributes[1:3]
attributes.boolean = attributes[4:7]
attributes.integer = attributes[8:25]
attributes.numerical = c(attributes.boolean, attributes.integer)

# Treatment of outliers
# ---------------------

# Boxplot to analyze the outliers of all integer attributes. Boolean attributes
# do not need be analyzed.
save.boxplot(
    data[, attributes.integer],
    main='[All] Integer attributes',
    names=range(len(attributes.integer))
)

# As we can see from the above plot that some attributes has outliers in the
# data. Let's analyze all them individually using boxplot and scatterplot.
for (attribute in attributes.integer) {
    i = index(attribute, attributes.integer)
    title = format.string('[%d] %s', i, attribute)
    values = data[, attribute]
    save.plot(values, main=title)
    save.boxplot(values, main=title)
}

# Filter to define a value limit for each integer attribute based on the
# analysis
attributes_filter = (
    data$Kill < 35 &
    data$Assists < 40 &
    data$Deaths < 30 &
    data$GoldEarned < 26250 &
    data$TotalDamageDealt < 500000 &
    data$MagicDamageDealt < 350000 &
    data$PhysicalDamageDealt < 450000 &
    data$TotalDamageDealtToChampions < 80000 &
    data$TotalDamageTaken < 80000 &
    data$MinionsKilled < 400 &
    data$NeutralMinionsKilled < 125 &
    data$CrowdControl < 10000 &
    data$WardsPlaced < 110 &
    data$TowerKills < 9 &
    data$LargestMultiKill < 6 &
    data$LargestKillingSpree < 21 &
    data$LargestCritStrike < 2500 &
    data$TotalHealAmount < 40000
)

# Filtering entire data to remove participants with large outliers
data = data[attributes_filter,]

# As data were looked up by participants, some matches were left with less than
# 10 participants. So, these inconsistent matches need to be removed.

# Number of participants by match id
participants_by_match = counter(data$matchId)

# Matches (IDs) that do not contain all 10 participants
inconsistent_matches = names(participants_by_match[participants_by_match < 10])

# Removing inconsistent matches from data
data = data[!(data$matchId %in% inconsistent_matches),]

# Boxplot to analyze the integer attributes after the treatments
save.boxplot(
    data[, attributes.numerical],
    main='[All] attributes (pos)',
    names=range(len(attributes.numerical)))

# Saving the treated data
write.csv(data, file="data/ranked_matches_2015_no_largeoutliers.csv")

# Data normalization (z-score)
# ----------------------------

# Since the data attributes are of different varieties their scales are also
# different. In order to maintain uniform scalability we normalize the
# integer attributes using Z-score. Boolean attributes do not need be
# normalized.
data.normalized = cbind(
    data[, attributes.boolean],
    scale(data[, attributes.integer])
)

# Correlation analysis
# --------------------

# Correlation between attributes of normalized data
correlations = cor(data.normalized)
correlations = abs(correlations)  # the signal does not matter
diag(correlations) = NA  # correlation of a set with itself does not matter
write.csv(correlations, file = "analysis/correlations.csv", sep =",")

# Boxplot to analyze attributes correlation
save.boxplot(
    correlations,
    main='Attributes correlation',
    names=range(ncol(correlations))
)

# Automatic attribute selection
-------------------------------

attributes.autoselection = attribute_selection(correlations)

# > attributes.autoselection
# [1] "Win"                         "Kills"
# [3] "Deaths"                      "GoldEarned"
# [5] "TotalDamageDealt"            "PhysicalDamageDealt"
# [7] "TotalDamageDealtToChampions" "TotalDamageTaken"
# [9] "MinionsKilled"               "WardsPlaced"
# [11] "TowerKills"                  "LargestMultiKill"
# [13] "LargestKillingSpree"         "LargestCritStrike"

# Manual attribute selection
# --------------------------

# Manual selection of the attributes based on correlation analysis
attributes.selection = c(
    "Kills",
    "Deaths",
    "GoldEarned",
    "TotalDamageDealt",
    "MagicDamageDealt",
    "PhysicalDamageDealt",
    "TotalDamageDealtToChampions",
    "TotalDamageTaken",
    "MinionsKilled",
    "WardsPlaced",
    "TowerKills",
    "LargestMultiKill",
    "LargestKillingSpree",
    "LargestCritStrike"
)

# Top 3 correlated attributes based on the mean of correlations for each one
attributes.topcorrelated = cor.rank()[1:3]
# > attributes.topcorrelated
# [1] "GoldEarned"                  "TotalDamageDealt"
# [3] "TotalDamageDealtToChampions"

# Reducing the dimensionality of the normalized data using attribute selection
ldata = data.normalized[, attributes.selection]

# K-means analysis
# ----------------

# To find the k number of clusters we can use the method that finds the "knee"
# of the error curve, which tries to find an appropriate number of clusters
# analyzing the curve of a generated graph from a test (based on k-means)
# conducted for each possible.

# Stores the total within sum of square resultant of k-means test for each k
twss = c()

# Testing k <= 50 clusters
for(k in 1 : 50)
    twss[k] = sum(kmeans(ldata, centers = k, algorithm = 'Lloyd')$withinss)

# Plot to analyze the knee resultant of the test for each k
save.plot(tss, main="K clusters", type="b", xlab="k", ylab="Total wihtinss")

# What is the best? k==8 or k==11?

# Extra k-means (configurations) tests
# ----------------------------------

fit = kmeans(ldata, 11, algorithm='Lloyd')
# (between_SS / total_SS =  61.2 %)
# Warning: did *not* converge in specified number of iterations

fit2 = kmeans(ldata, 11, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  61.9 %)

fit3 = kmeans(ldata, 8, algorithm='Lloyd')
# (between_SS / total_SS =  57.2 %)
# Warning: did *not* converge in specified number of iterations

fit4 = kmeans(ldata, 8, algorithm='Lloyd', iter.max=150)
# (between_SS / total_SS =  57.7 %)

# fit4 has the best trade-off

# Variance of each cluster found by fit4
fit4$withinvar = 1 / (fit4$size - 1) * fit4$withinss

# Saving the fit4
write.csv(fit4$cluster, file="analysis/cluster/testes/fit4$cluster.csv")
write.csv(fit4$centers, file="analysis/cluster/testes/fit4$centers.csv")
write.csv(fit4$size, file="analysis/cluster/testes/fit4$size.csv")
write.csv(fit4$totss, file="analysis/cluster/testes/fit4$totss.csv")
write.csv(fit4$tot.withinss, file="analysis/cluster/testes/fit4$tot.withinss.csv")
write.csv(fit4$withinss, file="analysis/cluster/testes/fit4$withinss.csv")
write.csv(fit4$betweenss, file="analysis/cluster/testes/fit4$betweenss.csv")
write.csv(fit4$iter, file="analysis/cluster/testes/fit4$iter.csv")
write.csv(fit4$ifault, file="analysis/cluster/testes/fit4$ifault.csv")
write.csv(fit4$withinvar, file="analysis/cluster/testes/fit4$withinvar.csv")

# Scatterplot of clusterized data
# -------------------------------

# Clusplot of clusterized data (n==80 rows)
clusplot(ldata[1:80,], fit4$cluster[1:80], color=TRUE, shade=TRUE, labels=2, lines=0)
legend("bottomleft", legend = paste("Group", 1:8), pch=1, col=1:8)

# Scatterplot of all attributes # TODO put in png file
plot(ldata, col=fit4$cluster, pch=15)

# Only most correlated attributes # TODO put in png file
plot(ldata[, attributes.topcorrelated], col=fit4$cluster, pch=15)

# Associating each participant tuple with its cluster
ldata2 = cbind(ldata, Cluster=fit4$cluster)

# Spliting clusterized data between winners and losers
vencedores = ldata2[ldata2$Win == 1,]
perdedores = ldata2[ldata2$Win == 0,]

# Scatterplot of each partition # TODO put in png file
plot(vencedores[, attributes.topcorrelated], col=vencedores$Cluster, pch=15)
plot(perdedores[, attributes.topcorrelated], col=perdedores$Cluster, pch=15)

# PCA of clusterized data
--------------------------

# Principal component indices to filter, based on top correlated attributes
pca_indices = index(attributes.topcorrelated[1:3], names(ldata))

# 3D scatterplot of most correlated attributes # TODO put in png file
scatterplot3d(
    prcomp(ldata, center=TRUE)$x[, pca_indices],
    pch=fit4$cluster,
    type="h",
    angle=95,
    color=fit4$cluster
)

# 3D scatterplot of most correlated attributes of winners # TODO put in png file
scatterplot3d(
    prcomp(vencedores[,1:(ncol(vencedores)-1)], center=TRUE)$x[, pca_indices],
    pch=vencedores$Cluster,
    type="h",
    angle=95,
    color=vencedores$Cluster
)

# 3D scatterplot of most correlated attributes of losers # TODO put in csv file
scatterplot3d(
    prcomp(perdedores[, 1:(ncol(perdedores)-1)], center=TRUE)$x[, pca_indices],
    pch=perdedores$Cluster,
    type="h",
    angle=95,
    color=perdedores$Cluster
)

# Summarying partitions
-----------------------

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
tmp = data[, attributes.selection]
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

### H1-0: Não existe diferença entre as distribuições dos clusters encontrados no modelo de aprendizagem

# normalized relative weight and normalized principal Eigen vector [Kardi Teknomo]

data.normalized_relative_weight = data[,4:25]
data.normalized_relative_weight = cbind(data.normalized_relative_weight[,c(1,5)], data.normalized_relative_weight[,7:14], data.normalized_relative_weight[,17:21])
data.normalized_relative_weight = data.normalized_relative_weight[,2:ncol(data.normalized_relative_weight)]

data.normalized_relative_weight = data.matrix(data.normalized_relative_weight)
data.normalized_relative_weight = data.normalized_relative_weight / sum(data.normalized_relative_weight)

ttt = data.normalized_relative_weight + 0.0
cbind(
   ttt[,1]/sum(ttt[,1]),
   ttt[,2]/sum(ttt[,2]),
   ttt[,3]/sum(ttt[,3]),
   ttt[,4]/sum(ttt[,4]),
   ttt[,5]/sum(ttt[,5]),
   ttt[,6]/sum(ttt[,6]),
   ttt[,7]/sum(ttt[,7]),
   ttt[,8]/sum(ttt[,8]),
   ttt[,9]/sum(ttt[,9]),
   ttt[,10]/sum(ttt[,10]),
   ttt[,11]/sum(ttt[,11]),
   ttt[,12]/sum(ttt[,12]),
   ttt[,13]/sum(ttt[,13]),
   ttt[,14]/sum(ttt[,14])
)

data.normalized_relative_weight = cbind(
   ttt[,1]/sum(ttt[,1]),
   ttt[,2]/sum(ttt[,2]),
   ttt[,3]/sum(ttt[,3]),
   ttt[,4]/sum(ttt[,4]),
   ttt[,5]/sum(ttt[,5]),
   ttt[,6]/sum(ttt[,6]),
   ttt[,7]/sum(ttt[,7]),
   ttt[,8]/sum(ttt[,8]),
   ttt[,9]/sum(ttt[,9]),
   ttt[,10]/sum(ttt[,10]),
   ttt[,11]/sum(ttt[,11]),
   ttt[,12]/sum(ttt[,12]),
   ttt[,13]/sum(ttt[,13]),
   ttt[,14]/sum(ttt[,14])
)

# dados normalizados pelo peso relativo
kruskal.test(rowSums(data.normalized_relative_weight), fit4$cluster)

    Kruskal-Wallis rank sum test

data:  rowSums(data.normalized_relative_weight) and fit4$cluster
Kruskal-Wallis chi-squared = 70612, df = 7, p-value < 2.2e-16

# dados normalizado z-score
kruskal.test(rowSums(ldata), fit4$cluster)

    Kruskal-Wallis rank sum test

data:  rowSums(ldata) and fit4$cluster
Kruskal-Wallis chi-squared = 70036, df = 7, p-value < 2.2e-16

# analisando pela soma dos quadrados
wilcox.test(fit4$withinss, conf.int=T)

    Wilcoxon signed rank test

data:  fit4$withinss
V = 36, p-value = 0.007813
alternative hypothesis: true location is not equal to 0
95 percent confidence interval:
 49032.26 73588.02
sample estimates:
(pseudo)median
      62429.24

### H1-1: Para cada cluster encontrado, existe diferença entre as medianas dos jogadores vitoriosos e perdedores

x = rowSums(vencedores[ vencedores$Cluster == 1,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 1,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 3275300, p-value = 0.006096
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 2,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 2,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 8345600, p-value = 0.00907
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 3,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 3,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 9178000, p-value = 0.0000000000008951
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 4,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 4,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 30907000, p-value = 0.3918
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 5,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 5,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 14623000, p-value < 0.00000000000000022
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 6,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 6,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 23422000, p-value < 0.00000000000000022
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 7,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 7,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 1088700, p-value = 0.0002433
alternative hypothesis: true location shift is not equal to 0

x = rowSums(vencedores[ vencedores$Cluster == 8,  ][,1:14])
y = rowSums(perdedores[ perdedores$Cluster == 8,  ][,1:14])
wilcox.test(x , y, paired=FALSE)
    Wilcoxon rank sum test with continuity correction
data:  x and y
W = 17823000, p-value = 0.002865
alternative hypothesis: true location shift is not equal to 0


# References
# [Kardi Teknomo] Kardi Teknomo. ANALYTIC HIERARCHY PROCESS (AHP) TUTORIAL. https://docs.google.com/file/d/0BxYU82vErc8xYS1PendBeHlpSkk/edit?usp=sharing
# http://stats.stackexchange.com/questions/49521/how-to-find-variance-between-multidimensional-points
# http://www.edureka.co/blog/clustering-on-bank-data-using-r/
# http://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
# http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
# http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
# http://blog.melchua.com/2012/09/07/r-basics-accessing-elements-of-an-object/
