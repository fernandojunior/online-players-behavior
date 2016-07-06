# helper functions

import_package('corrplot', attach=TRUE)  # corrplot
import('fun', attach=TRUE)
import('correlation', attach=c('correlation_matrix'))

#' Compute a vector of breakpoints, ie the cutoff points to bin x.
#'
#' @param x Vector
#' @param method {function optional nclass.Sturges} Method to compute number
#'     of bins.
#'
#' @return Breakpoints
#'
#' @examples
#'    generate_breaks(range(1000))
#'    #> [1]    0  100  200  300  400  500  600  700  800  900 1000
#'
#' @references
#'     https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width
#'     http://www.r-bloggers.com/basics-of-histograms/
#'     https://stat.ethz.ch/pipermail/r-help/2014-March/372559.html
generate_breaks = function (x, method=nclass.Sturges) {
    x.min = min(x)
    x.max = max(x)
    bins = method(x)
    return(pretty(x.min + range(0, bins) * (x.max - x.min)/bins, n=bins))
}

#' Discretize a numeric vector x by grouping into a smaller number of bins.
#'
#' It can be used to transform x into a categorical one.
#'
#' @param x {numeric} Vector to discretize.
#' @param breaks {numeric optional generate_breaks(x)} Cutoff points to bin x.
#'
#' @examples
#'     discretize(range(10))
#'     #> [1] (0,2]  (0,2]  (2,4]  (2,4]  (4,6]  (4,6]  (6,8]  (6,8]  (8,10] (8,10]
#'     #> Levels: (0,2] (2,4] (4,6] (6,8] (8,10]
#' @references
#'     https://en.wikipedia.org/wiki/Data_binning
#'     http://www.mathworks.com/help/matlab/ref/discretize.html
discretize = function (x, breaks=generate_breaks(x)) {
    return(cut(x, breaks=breaks))
}

#' Discriminate a dataset x by y to (distinct) count its subsets.
counter_by = function (x, y) {
    x.domain = sort(unique(x))
    y.domain = sort(unique(y))
    z = matrix(0, nrow=length(y.domain), ncol=length(x.domain))
    colnames(z) = x.domain
    rownames(z) = y.domain
    for (i in range(length(y.domain))) {
        counts = counter(x[y == y.domain[i]])
        for (j in names(counts)) {
            z[i, j] = counts[j]
        }
    }
    return(z)
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
#'     betweenss = abs(tot.withinss - totss)
#'     (betweenss / totss) == betweenss.rate(kmeans(data, 5))
#'     #> [1] TRUE
betweenss.rate = function (fit) {
    return(fit$betweenss / fit$totss)
}

#' Perform a cluster analysis on a data matrix x for each k = {1:kmax}
#' number of clusters.
#'
#' The analysis is based on k-means. K-means clustering model aims to
#' partition the data into k clusters, so as to minimize the sum of squared
#' error (SSE or SS). To find the optimal k we can use the the knee of the
#' error curve method, which tries to find an appropriate number of clusters
#' analyzing the curve of a generated graph from a clustering conducted for
#' each possible.
cluster_analysis = function (data, kmax=20, main='Error curve', show=TRUE) {
    # K-means clustering for each k
    fits = t(map(
        function(k) kmeans(data, k, algorithm='Lloyd', iter.max=200),
        range(kmax)
    ))

    # Total within-cluster SSE for each k-means clustering
    twss = Row(function(fit) fit$tot.withinss, fits)
    twss.prop = twss/twss[1]

    if (show == TRUE) {
        # Plot to analyze the knee of error curve
        ylab = 'tot.withinss(k)/tot.withinss(k=1)'
        plot(twss.prop, main=main, xlab='k', ylab=ylab, ylim=c(0, 1))
        features = colnames(data)
        legend_ = paste(features, collapse='\n')
        legend('topright', legend=legend_, bty="n", cex=0.7)
    }

    result = list()
    result$fits = fits
    result$twss = twss
    result$twss.prop = twss.prop
    return(result)
}

#' Perform ntests cluster analysis on a matrix x for each k = {1:kmax}.
#'
#' If ncol != NULL, ncol columns of x are chosen randomly to the analysis.
#' In the end, the tests are summarized by mean.
many_cluster_analysis = function (x, ncol=NULL, kmax=10, ntests=20) {
    tests = matrix(0, nrow=ntests, ncol=kmax)
    for (i in range(ntests)) {
        cols = if (is.null(ncol)) colnames(x) else sample(colnames(x), ncol)
        tests[i, ] = cluster_analysis(x[, cols], kmax=kmax, show=FALSE)$twss.prop
    }
    tests.summary = apply(tests, 2, mean)

    ylab = 'mean(tot.withinss(k)/tot.withinss(k=1))'
    legend_ = strf('random features: %s, tests: %s', ncol, ntests)
    plot(tests.summary, ylim=c(0, 1), ylab=ylab, xlab='k')
    legend('topright', legend=legend_, bty="n", cex=0.7)

    return(apply(tests, 2, mean))  # summary
}

# correlation functions

#' Perform a correlation analysis on a matrix x.
#'
#' @todo Examples.
#'
#' @references
#'     https://rpubs.com/gaston/dendrograms
#'     https://cran.r-project.org/web/packages/corrplot
correlation_analysis = function (x) {
    # Correlation matrix of x using Spearman method, which does not require the
    # features follow a normal distribuition or linear correlation.
    correlations = correlation_matrix(x, method='spearman', exact=FALSE)

    par(mfrow=c(1, 2))

    # Cluster dendrogram plot to analyze the affinity of each attribute based
    # on the correlation matrix.
    plot(
        hclust(dist(correlations$estimates)),
        main='[Correlation] Features Dendrogram'
    )

    # Heatmap plot of the correlation matrix. p.values greater than significance
    # level at 0.05 are indicated.
    corrplot(
        round(correlations$estimates, 1),
        main='[Correlation] Features heatmap',
        p.mat=correlations$p.values,
        sig.level=0.05,
        method='number',
        order='alphabet'
    )

    return(correlations)
}

# math functions

#' Return the sum of square error of a vector x: (n - 1) * var(x).
#'
#' If VAR == TRUE, x is a variance value (or a list) of a sample data with n
#' length.
#'
#' @examples
#'     x = c(1, 2, 3, 4, 5)
#'     length(x)
#'     #> [1] 5
#'     var(x)
#'     #> [1] 2.5
#'     ss(x)  # sum of square of x
#'     #> [1] 10
#'     ss(2.5, 5, VAR=TRUE)  # pass only the variance and the size of x
#'     #> [1] 10
#'     ss(c(2.5, 2.5), 5, VAR=TRUE)  # pass variances of a m. data n == 5
#'     #> [1] 10 10
ss = function (x, n=NA, VAR=FALSE) {
    if (VAR == FALSE) {
        if (is.vector(x))
            n = length(x)
        if (is.matrix(x))
            n = nrow(x)
        x = var(x)
    }
    return ((n - 1) * x)
}

#' Select the features of an data matrix x based on min > f(x) < max
filter_features = function (x, f, min=NULL, max=NULL) {
    y = apply(x, 2, f)

    features = colnames(x)
    if (is.null(min) && !is.null(max))
        features = y < max
    else if (!is.null(min) && is.null(max))
        features = y > min
    else
        features = y > min & y < max

    features = names(features[features == TRUE])
    return(features)
}

# plots

#' Save a plot, renderized by a f function, in a PNG file.
#'
#' @param f Function to render the plot.
#' @param filename Name of the output file.
#' @param path Optional prefix path for filename
#' @param width Width of the plotting window, in inches.
#' @param height Height of the plotting window, in inches.
#' @param close Indicate if the plotting window must be closed or not.
save_plot = function (f, filename, path='', width=9, height=9, close=FALSE) {
    x11(width=width, height=height)
    path = if (path != '') strf('%s/', path) else path
    filename = strf('%s%s.png', path, filename)
    result = f()
    savePlot(filename=filename, type='png')
    print(strf('Plot saved at %s', filename))
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
    legend("topright", legend=y.domain, col = colors,  lwd = 5, title = "y")
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
    legend("topright", legend=y.domain, col = colors,  lwd = 5, title = "y")
}

#' 3-D visualization of 3 principal components of a matrix x
pca_plot = function (x, ...) {
    scatterplot3d(prcomp(x, center=TRUE)$x[, 1:3], ...)
}

#' Summary each col of x by y using a function f to render single plots in one.
#'
#' @param x Matrix
#' @param y Grouping vector
#' @param f Function to summarize the columns of x
#' @param ... Optional aruments for `plot`
#'
#' @examples
#'     a = c(462, 842, 912, 531)
#'     b = c(21, 493, 549, 684)
#'     c = (a + b)/2
#'     g = c(1, 1, 3, 3)
#'     x = data.frame(a, b, c)
#'     aggregate_plot(x, g, mean)
aggregate_plot = function (x, y, f, ...) {
    cols = or(colnames(x), 1:ncol(x))
    cols_range = 1:length(cols)
    cols_id = map(function (x) strf('#%s', x), cols_range)
    cols_pretty = map(function(i) strf('%s#%s', cols[i], i), cols_range)
    agg = pairify(aggregate(. ~ y, x[, cols], f)[, cols])
    agg$key = Map(function (key) indexof(key, cols), agg$key)
    args = list(...)
    args$x = agg$key
    args$y = agg$value
    args$col=agg$id
    args$pch = paste(agg$id)
    args$xlab = paste(cols_pretty, collapse=', ')
    args$ylab = or(args$ylab, as.character(substitute(f)))
    args$xaxt = "n"
    do.call(plot, args)
    axis(1, at=cols_range, labels=cols_id)
    position = "bottomright"
    colors = unique(args$col)
    title = 'Group = y'
    legend(position, legend=colors, col=colors, lwd=5, title=title, cex=0.8)
}

# parsers ---------------------------------------------------------------------

#' Transform a matrix x into a key/value pairs matrix
#'
#' @seealso tidyr::spread
#'
#' @examples
#'     a = c(462, 842, 912)
#'     b = c(21, 493, 549)
#'     x = data.frame(a, b)
#'     pairify(x)
#'     #>   id key value
#'     #> 1  1   a   462
#'     #> 2  2   a   842
#'     #> 3  3   a   912
#'     #> 4  1   b    21
#'     #> 5  2   b   493
#'     #> 6  3   b   549
pairify = function (x) {
    parse = function (y) {
        return(data.frame(id=1:nrow(x), key=y, value=x[, y], row.names=NULL))
    }
    return(data.frame(do.call(rbind, Map(parse, colnames(x))), row.names=NULL))
}
