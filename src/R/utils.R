# helper functions

import_package('corrplot', attach=TRUE)  # corrplot
import_package('scatterplot3d', attach=TRUE)  # scatterplot3d
import('fun', attach=TRUE)
import('correlation', attach=c('correlation_matrix'))

#' Perform simple descriptive statistics
descriptive_statistics = function (data, features) {
    statistics = round(t(apply(data[, features], 2, function(col) {
        return(cbind(min(col), max(col), mean(col), median(col), var(col), sd(col)))
    })), digits=2)
    colnames(statistics) = c('min', 'max', 'mean', 'meadian', 'var', 'sd')
    return(statistics)
}

#' Viz clustered data exploring: Scatter and PCA plot for the first three features.
#'
#' In general, we can observe the k clusters found in k-means clustering. We can also observe that some clusters are
#' more perceptible than others when the clustered data is discriminated between winners and losers.
cluster_data_viz = function (data, features, target_feature, cluster_feature, pca_lim=c(1, -1)) {
    winners = data[data[, target_feature] == 1, ]
    losers = data[data[, target_feature] == 0, ]

    # Clustered data plot for the first three features
    render_plot(function () {
        main = 'Exploring - Scatter plot'
        plot(data[, features[1:3]], main=main, col=data[, cluster_feature])
    }, '../output/exploring-scatterplot-player')

    # Only winners
    render_plot(function () {
        main = 'Exploring clusters - Scatter plot for winners (true targets)'
        plot(winners[, features[1:3]], main=main, col=winners[, cluster_feature])
    }, '../output/exploring-scatterplot-winners-player')

    # Only losers
    render_plot(function () {
        main = 'Exploring clusters - Scatter plot for losers (false targets)'
        plot(losers[, features[1:3]], main=main, col=losers[, cluster_feature])
    }, '../output/exploring-scatterplot-losers-player')

    # 3-D visualization of 3 principal components of the clustered data
    render_plot(function () {
        par(mfrow=c(1, 3))
        # max = max(data[, features[1:3]])
        #lim = c(-max, max) # TODO review
        lim = pca_lim
        angle = 95
        pca_plot(data[, features[1:3]], main='PCA', color=data[, cluster_feature], angle=angle, xlim=lim, ylim=lim,
                 zlim=lim)
        pca_plot(winners[, features[1:3]], main='PCA winners', color=winners[, cluster_feature], angle=angle, xlim=lim,
                 ylim=lim, zlim=lim)
        pca_plot(losers[, features[1:3]], main='PCA losers', color=losers[, cluster_feature], angle=angle, xlim=lim,
                 ylim=lim, zlim=lim)
    }, '../output/exploring-pca-player', width=18, height=9)
}

#' Statistical analysis of cluster analysis by discriminating a binary target
#'
#' Hypothesis 1. H1-0: There is no difference between the distributions of the clusters found in the learning model;
#' H1-1 There is difference between the distributions of the clusters found in the learning model. Test: Kruskal-Wallis
#' rank sum test
#'
#' Hypothesis 2. H2-0: For each cluster found in the learning model there is no difference between the medians of the
#' winning players and losing players; (H2-1) for each cluster found there is difference between the medians of the
#' winning players and losing players. Test: Wilcoxon rank sum test with continuity correction
cluster_statistical_analysis = function (data, features, target_feature, cluster_feature) {
    cluster_domain = sort(unique(data[, cluster_feature]))
    cluster_values = data[, cluster_feature]
    winners = data[data[, target_feature] == 1, ]
    losers = data[data[, target_feature] == 0, ]

    # Alternative hypothesis true: p.value < 0.05
    h1 = kruskal.test(rowSums(data[, features]), cluster_values)

    # Alternative hypothesis true for each cluster: p.value < 0.05
    h2.p.values = values(Map(function (k) {
        x = rowSums(winners[winners[, cluster_feature] == k, features])
        y = rowSums(losers[losers[, cluster_feature] == k, features])
        # TODO print(paste(shapiro.test(x)$p.value, shapiro.test(y)$p.value))
        # TODO print(t.test(x, y, paired=FALSE)$p.value)
        wilcox.test(x , y, paired=FALSE)$p.value
    }, cluster_domain))

    render_plot(function () {
        par(mfrow=c(1, 2))
        plot(1, h1$p.value, main='Hypothesis - H1', xlab='h1', ylab='p.value')
        plot(h2.p.values, main='Hypothesis - H2', xlab='k', ylab='p.values')
    }, '../output/hypothesis-player', width=16, height=9)
}

#' Given a data set x, summarize the mean for each feature by label.
#' TODO cluter boxplot
centroid_analysis = function (data, features, plot_name) {
    winners = data[data$winner == 1, ]
    losers = data[data$winner == 0, ]

    max = max(data[, features])
    min = -max

    render_plot(function () {
        par(mfrow=c(3,1))
        lim = c(min, max)
        plot_by(data[, features], data$label, mean, ylim=lim)
        plot_by(winners[, features], winners$label, mean, ylim=lim)
        plot_by(losers[, features], losers$label, mean, ylim=lim)
    }, plot_name, width=16, height=9)
}

#' Balance data using undersample method based on target (binary) and label (multiclass) features
balance = function (data, target, label, prop=0.8) {
    # Discriminate clustered data between target (winners and losers) to analyse sizes
    winners = data[data[, target] == 1, ]
    losers = data[data[, target] == 0, ]
    clusters_size = cbind(all=table(data[, label]), winners=table(winners[, label]), losers=table(losers[, label]))

    relative_clusters_size = cbind(
        winners=clusters_size[, 'winners'] / clusters_size[, 'all'],
        losers=clusters_size[, 'losers'] / clusters_size[, 'all']
    )

    # Check if there are outliers in relative clusters size
    # boxplot(values(relative_clusters_size))$stats

    # Min cluster size between winners and losers to undersample data
    min_clusters_size = round(min(table(winners[, 'label']), table(losers[, 'label'])) * prop)

    cluster_size_analysis = list(size=clusters_size, relative_size=relative_clusters_size, min_size=min_clusters_size)

    # balance data based on cluster size analysis
    balanced = rbind(undersample(winners, label, min_clusters_size), undersample(losers, label, min_clusters_size))

    print(cluster_size_analysis)

    return(list(cluster_size_analysis=cluster_size_analysis, data=balanced))
}

#' Normalize a dataset x
normalize = function (x) {
    return((x-min(x))/(max(x)-min(x)))
}

#' Aggregate a dataset with a function FUN=counter
#'
#' Same as `aggregate(x, by=y, counter)` or `aggregate(x ~ y, data, counter)`
#'
#' @seealso aggregate
counter_by = function (...) {
    return(Curry(aggregate, FUN=counter)(...))
}

#' Undersample a dimensional dataset x for each class in a categorical target
#' column with a specified sample size
undersample = function (x, target, size) {
    y = x[, target]
    indices = values(Map(function (k) {
        return(sample(rownames(x[x[, target] == k, ]), size))
    }, unique(y)))
    return(x[rownames(x) %in% indices, ])
}

# cluster functions

#' Between-cluster Sum of Square Error rate of a k-means fit.
#'
#' It represents the total SSE rate minimized from the data after
#' partitioning in k clusters.
#'
#' @todo Refactor `betweenss.rate` to betweenss.prop
#'
#' @examples
#'     data = sample(range(1000), 100)
#'     totss = sum(ss(data))
#'     tot.withinss = sum(kmeans(data, 5)$withinss)
#'     betweenss = abs(totss - tot.withinss)
#'     (betweenss / totss) == betweenss.rate(kmeans(data, 5))
#'     #> [1] TRUE
betweenss.rate = function (fit) {
    return(fit$betweenss / fit$totss)
}

#' Perform a cluster analysis on a matrix x.
#'
#' Apply `kmeans` on x for each k = {1:kmax} number of clusters. Also render a
#' knee of error curve plot based on the within cluster sum of squared erros in
#' order to find the optimal k.
#'
#' K-means clustering model aims to partition the data into k clusters, so as
#' to minimize the sum of squared error (SSE or SS). To find the optimal k we
#' can use the the knee of the error curve method, which tries to find an
#' appropriate number of clusters analyzing the curve of a generated graph from
#' a clustering conducted for each possible.
#'
#' @param x Numeric matrix
#' @param kmax Max number of cluster
#' @param show Render (TRUE) or not (FALSE) the knee of error curve plot
#'
#' @return $fits: A list with the `kmeans` results for each k = {1:kmax} as
#'     size, withinss, tot.withinss, etc.; $twss: Numeric vector with the total
#'     within cluster sum of squared erros `tot.withinss` of the fits
#'
#' @seealso kmeans
cluster_analysis = function (x, kmax=20, show=TRUE) {
    # K-means clustering for each number of k = {1:kmax}
    fits = Map(function(k) {
        fit = kmeans(x, k, algorithm='Lloyd', iter.max=1500)
        fit$betweenss.rate = betweenss.rate(fit)  # Between-cluster SSE rate
        fit$k = length(fit$size)  # Number of clusters
        fit$withinvar = 1/(fit$size-1)*fit$withinss  # Variance in each cluster
        return(fit)
    }, 1:kmax)

    # Total within-cluster SSE for each k-means clustering
    twss = values(Map(function (fit) fit$tot.withinss, fits))

    # Relative TWSS
    twss.rel = twss/twss[1]

    # Plot to analyze the knee of error curve
    if (show == TRUE) {
        main = 'K-means - Error curve'
        ylab = 'tot.withinss(k)/tot.withinss(k=1)'
        legends = paste(colnames(x), collapse='\n')
        plot(twss.rel, main=main, xlab='k', ylab=ylab, ylim=c(0, 1))
        legend('topright', legend=legends, bty='n', cex=0.7)
    }

    return(list(fits=fits, twss=twss, 'twss.rel'=twss.rel))
}

#' Perform ntests cluster analysis on a matrix x for a given kmax to find the
#' mean error curve. For each ntests, a random feature subspace is selected to
#' be clusterized and, then, compute the relative TWSS for k = {1, ... kmax}.
#' At the end, the relative TWSS for all tests are summarized by the mean.
#'
#' If ncol == NULL, all columns of x are chosen for the analysis.
bagging_cluster_analysis = function (x, ncol=NULL, kmax=10, ntests=20) {
    twss.rel = matrix(0, nrow=ntests, ncol=kmax)
    for (i in range(ntests)) {
        cols = if (is.null(ncol)) colnames(x) else sample(colnames(x), ncol)
        twss.rel[i, ] = cluster_analysis(x[, cols], kmax=kmax, show=FALSE)$twss.rel
    }
    twss.rel.mean = apply(twss.rel, 2, mean)

    main = 'K-menas - Mean error curve (bagging)'
    ylab = 'mean(tot.withinss(k)/tot.withinss(k=1))'
    legends = ''
    if (is.null(ncol))
        legends = strf('features: %s, tests: %s', ncol(x), ntests)
    else
        legends = strf('random features: %s, tests: %s', ncol, ntests)
    plot(twss.rel.mean, main=main, ylim=c(0, 1), ylab=ylab, xlab='k')
    legend('topright', legend=legends, bty='n', cex=0.7)

    return(twss.rel.mean)  # summary
}

#' Perform outlier analysis on a matrix x.
#'
#' Render a boxplot for each column to analyze the outliers of x. Also
#' indentify outliers with `find_outliers`.
#'
#' @return Results from find_outliers
#'
#' @seealso find_outliers
outlier_analysis = function (x, factor=1.5) {
    cols = or(colnames(x), 1:ncol(x))
    ncols = 6
    nrows = ceiling(length(cols)/ncols)
    par(mfrow=c(nrows, ncols))
    each(function(i) boxplot(x[, i], main=i), cols)
    return(find_outliers(x, factor=factor))
}

#' Perform a correlation analysis on a matrix x using Spearman method.
#'
#' Estimates the correlation matrix and render dendrogram and corrplot for x
#' to analyze the similarity of its columns.
#'
#' @todo Examples.
#'
#' @param x Numeric matrix
#'
#' @return Results from `correlation_matrix`.
#'
#' @seealso correlation_matrix, dendrogram, corrplot
#'
#' @references
#'     https://rpubs.com/gaston/dendrograms
#'     https://cran.r-project.org/web/packages/corrplot
correlation_analysis = function (x) {
    correlations = correlation_matrix(x, method='spearman', exact=FALSE)
    correlations$estimates[is.na(correlations$estimates)] <- 0

    par(mfrow=c(1, 2))
    dendrogram(correlations$estimates)
    corrplot(
        round(correlations$estimates, 1),
        p.mat=correlations$p.values,
        sig.level=0.05,
        method='number',
        order='alphabet'
    )

    return(correlations)
}

# plots

#' Render a plot in a function f to a PNG file.
#'
#' @param f Function with a plot.
#' @param filename {character optional 'Rplot'} Name of the output file.
#' @param width {numeric optional 9} Width of the plotting window in inches.
#' @param height {numeric optional 9} Height of the plotting window in inches.
#' @param close {logic optional FALSE} Indicate if the plotting window must be
#'     closed or not. Configurable with `RENDER_PLOT_CLOSE` variable
#' @param save {logic optional TRUE} Indicate if the plotting window must be
#'     saved or not. Configurable with `RENDER_PLOT_SAVE` variable
#'
#' @return Results from f
#'
#' @examples
#'     render_plot(function () plot(1:10))
#'     #> [1] "Plot saved at Rplot.png"
#'     #> NULL
#'     render_plot(function () plot(1:10), filename='test')
#'     #> [1] "Plot saved at test.png"
#'     #> NULL
#'     render_plot(function () plot(1:10), filename='test2.png', height=4)
#'     #> [1] "Plot saved at test2.png"
#'     #> NULL
#'     render_plot(function () plot(1:10), save=FALSE, width=10, height=4)
#'     #> NULL
#'     render_plot(function () plot(1:10), save=FALSE, close=TRUE)
#'     #> NULL
#'     RENDER_PLOT_SAVE = FALSE
#'     RENDER_PLOT_CLOSE = TRUE
#'     render_plot(function () plot(1:10))
#'     #> NULL
render_plot = function (f, filename='Rplot', width=9, height=9, close=FALSE,
                        save=TRUE) {
    if (exists("RENDER_PLOT_CLOSE") && !is.null(RENDER_PLOT_CLOSE))
        close = RENDER_PLOT_CLOSE
    if (exists("RENDER_PLOT_SAVE") && !is.null(RENDER_PLOT_SAVE))
        save = RENDER_PLOT_SAVE
    x11(width=width, height=height)
    result = f()
    if (!is.null(filename) && save == TRUE) {
        if (!endsWith(filename, '.png'))
            filename = strf('%s.png', filename)
        savePlot(filename=filename, type='png')
        print(strf('Plot saved at %s', filename))
    }
    if (close == TRUE)
        dev.off()
    return(result)
}

#' Discriminate a dataset x by y to draw cobinated histograms plots.
#'
#' @references
#'     http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
chist = function (x, y, palette=rainbow) {
    breaks = generate_breaks(x)
    xlim = c(min(breaks), max(breaks))
    y.domain = unique(y)
    y.counts = table(discretize(x, breaks))
    ylim = c(min(y.counts), max(y.counts))
    colors = adjustcolor(palette(length(y.domain)), alpha.f = 0.3)

    for (i in range(length(y.domain))) {
        value = y.domain[i]
        subset = x[y == value]
        col = colors[i]
        if (i == length(y.domain))
            hist(subset, col=col, xlim=xlim, ylim=ylim, breaks=breaks, xlab='x', add=T)
        else
            hist(subset, col=col, xlim=xlim, ylim=ylim, breaks=breaks, xlab='x', main='')
    }
    legend('topright', legend=y.domain, col = colors,  lwd = 5, title = 'y')
}

#' Discriminate a dataset x by y to draw multiple histogram plots.
mhist = function (x, y, palette=rainbow) {
    breaks = generate_breaks(x)
    xlim = c(min(breaks), max(breaks))
    y.domain = unique(y)
    y.counts = table(discretize(x, breaks))
    ylim = c(min(y.counts), max(y.counts))
    colors = adjustcolor(palette(length(y.domain)), alpha.f = 0.3)

    par(mfrow=c(1, length(y.domain) + 1))
    hist(x, xlim=xlim, ylim=ylim, breaks=breaks, main='')
    for (i in range(length(y.domain))) {
        value = y.domain[i]
        subset = x[y == value]
        col = colors[i]
        xlab = strf('x[y == %s]', value)
        hist(subset, col=col, xlim=xlim, ylim=ylim, breaks=breaks, main='', xlab=xlab)
    }
    legend('topright', legend=y.domain, col = colors,  lwd = 5, title = 'y')
}

#' 3-D visualization of 3 principal components of a matrix x
pca_plot = function (x, ...) {
    scatterplot3d(prcomp(x, center=TRUE)$x[, 1:3], ...)
}

#' Render paired single plots in one of a summarized matrix x.
#'
#' @param x Matrix
#' @param y Grouping vector
#' @param f Function to summarize x
#' @param ... Optional aruments for `plot`
#'
#' @examples
#'     a = c(462, 842, 912, 531)
#'     b = c(21, 493, 549, 684)
#'     c = (a + b)/2
#'     g = c(1, 1, 3, 3)
#'     x = data.frame(a, b, c)
#'     plot_by(x, g, mean)
plot_by = function (x, y, f, ...) {
    cols = or(colnames(x), 1:ncol(x))
    labels = Map(function (x) strf('#%s', x), 1:length(cols))
    named_labels = Map(function(i) strf('%s#%s', cols[i], i), 1:length(cols))
    print(values(named_labels))
    grouped = pairify(aggregate(. ~ y, x, f)[, cols])
    grouped$key = Map(function (key) indexof(key, cols), grouped$key)
    args = list(...)
    args$x = grouped$key
    args$y = grouped$value
    args$col = sort(unique(y))
    args$pch = paste(grouped$id)
    args$xlab = paste(named_labels, collapse=', ')
    args$ylab = or(args$ylab, as.character(substitute(f)))
    args$xaxt = 'n'
    do.call(plot, args)
    axis(1, at=1:length(labels), labels=labels)
    position = 'bottomright'
    colors = unique(args$col)
    title = 'Group = y'
    legend(position, legend=colors, col=colors, lwd=5, title=title, cex=0.8)
}

#' Render a simple cluster dendrogram plot for x
#'
#' @examples
#'     plot(hclust(dist(sample(1:1000, 100))))
#'
#' @references
#'     https://rpubs.com/gaston/dendrograms
dendrogram = function (x) {
    return(plot(hclust(dist(x))))
}
