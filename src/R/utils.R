# helper functions

import_package('corrplot', attach=TRUE)  # corrplot
import_package('scatterplot3d', attach=TRUE)  # scatterplot3d
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

#' Aggregate a dataset with a function FUN=counter
#'
#' Same as `aggregate(x, by=y, counter)` or `aggregate(x ~ y, data, counter)`
#'
#' @seealso aggregate
counter_by = function (...) {
    return(Curry(aggregate, FUN=counter)(...))
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
        fit = kmeans(x, k, algorithm='Lloyd', iter.max=200)
        fit$betweenss.rate = betweenss.rate(fit)  # Between-cluster SSE rate
        fit$k = length(fit$size)  # Number of clusters
        fit$withinvar = 1/(fit$size-1)*fit$withinss  # Variance in each cluster
        return(fit)
    }, 1:kmax)

    # Total within-cluster SSE for each k-means clustering
    twss = values(Map(function (fit) fit$tot.withinss, fits))
    twss.prop = twss/twss[1]

    # Plot to analyze the knee of error curve
    if (show == TRUE) {
        main = 'K-means - Error curve'
        ylab = 'tot.withinss(k)/tot.withinss(k=1)'
        legends = paste(colnames(x), collapse='\n')
        plot(twss.prop, main=main, xlab='k', ylab=ylab, ylim=c(0, 1))
        legend('topright', legend=legends, bty='n', cex=0.7)
    }

    return(list(fits=fits, twss=twss))
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
    legends = strf('random features: %s, tests: %s', ncol, ntests)
    plot(tests.summary, ylim=c(0, 1), ylab=ylab, xlab='k')
    legend('topright', legend=legends, bty='n', cex=0.7)

    return(apply(tests, 2, mean))  # summary
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
    nrows = trunc(length(cols)/ncols)
    par(mfrow=c(nrows, ncols))
    each(function(i) boxplot(x[, i], main=i), cols)
    return(find_outliers(x, factor=factor))
}

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
