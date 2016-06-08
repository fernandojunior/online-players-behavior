# Constants

PLOT_DIR = '../output/'

# helper functions

counter = function (...) {
    "Alias for table."
    return(table(...))
}

indexof = function (e, l) {
    "Alias for match."
    return(match(e, l))
}

map = function (f, x) {
    "Apply a function f to a value x.

    If x is a vector or a list, it applies for each item.

    if x is a matrix or data frame, it applies for each item of each column.

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
    else if (is.matrix(x) | is.data.frame(x))
        apply(x, 2, function (y) map(f, y))
}

rowmap = function (f, x) {
    "Apply a function f for each row in a matrix or data frame x."
    return(apply(x, 1, f))
}

colmap = function (f, x) {
    "Apply a function f for each column in a matrix or data frame x."
    return(apply(x, 2, f))
}

range = function (...) {
    "Alias for seq.int. Override the built-in funtion."
    return(seq.int(...))
}

values = function (x) {
    "Return only the values of x.

    x can be a list, vector, matrix or data frame.
    "
    if (is.list(x))
        x = unlist(x)
    if (is.data.frame(x))
        x = data.matrix(x)
    return(unname(x))
}

generate_breaks = function (x, method=nclass.Sturges) {
    "Compute a vector of breakpoints automatically, ie the cutoff points to bin
    a dataset x.

    The default number of bins is based on Sturges formula.

    References:
    https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width
    http://www.r-bloggers.com/basics-of-histograms/
    https://stat.ethz.ch/pipermail/r-help/2014-March/372559.html
    "
    x.min = min(x)
    x.max = max(x)
    bins = method(x)
    return(pretty(x.min + range(0, bins) * (x.max - x.min)/bins, n=bins))
}

discretize = function (x, breaks=NULL) {
    "Group a numeric vector x into a smaller number of bins.

    It can be used to transform a numeric vector into a categorical one.

    Reference:
    https://en.wikipedia.org/wiki/Data_binning
    http://www.mathworks.com/help/matlab/ref/discretize.html?requestedDomain=www.mathworks.com
    "
    breaks = if (is.null(breaks)) generate_breaks(x) else breaks
    return(cut(x, breaks=breaks))
}

mcounter = function (x, y) {
    "Discriminate a dataset x by y to count its subsets.
    "
    x.domain = unique(x)
    y.domain = unique(y)
    z = matrix(0, nrow=length(y.domain), ncol=length(x.domain))
    colnames(z) = x.domain
    rownames(z) = y.domain
    for (i in range(length(y.domain))) {
        counts = table(x[y == y.domain[i]])
        for (j in names(counts)) {
            z[i, j] = counts[j]
        }
    }
    return(z)
}

# string functions

strf = function (...) {
    "Alias for sprintf."
    return(sprintf(...))
}

# math functions

ss = function (x, n=NA, VAR=FALSE) {
    "Return the sum of square error of a data set x: (n - 1) * var(x).

    If VAR == TRUE, x is a variance value (or a list) of a sample data with n
    length.

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

is.outlier = function (x, lower, upper) {
    "Return TRUE if x is an outlier, FALSE otherwise.

    It's based on lower and upper thresholds.

    x point can be multivariate (a list or vector). In this case, the
    thresholds also must be.
    "
    thresholds = rbind(lower, upper)
    if(length(x) > 0 & !is.null(colnames(thresholds)))  # x is multivariate
        x = x[colnames(thresholds)]
    if (any(x < thresholds['lower',]) | any(x > thresholds['upper',]))
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
    return(names(sort(cor.mean(correlation_matrix), decreasing=decreasing)))
}

cor.mtest = function(x, method='pearson', ...) {
    "Peform a cor.test between paired features of a multivariate data set x."
    features = colnames(x)
    n = length(features)

    basematrix = matrix(NA, n, n)
    colnames(basematrix) = rownames(basematrix) = features

    res = list()
    res$estimates = res$p.values = basematrix

    # A correlation of the diagonal correlation matrix is a correlation of a
    # data attribute with itself. So, the diagonal must be NA to prevent wrong
    # behaviors as in dendrogram and box plots.
    diag(res$estimates) = NA

    diag(res$p.values) = 0

    if (method == 'pearson') {
        res$conf.int = list()
        res$conf.int$lower = res$conf.int$upper = basematrix
        diag(res$conf.int$lower) = diag(res$conf.int$upper) = 1
    }

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp = cor.test(x[, i], x[, j], method=method, ...)
            res$estimates[i, j] = res$estimates[j, i] = tmp$estimate
            res$p.values[i, j] = res$p.values[j, i] = tmp$p.value
            if ('conf.int' %in% names(tmp)) {
                res$conf.int$lower[i, j] = res$conf.int$lower[j, i] = tmp$conf.int[1]
                res$conf.int$upper[i, j] = res$conf.int$upper[j, i] = tmp$conf.int[2]
            }
        }
    }

    return(res)
}

cor.plot = function (x, ...) {
    "Alias for corrplot."
    if (!require('corrplot')) {
        install.packages('corrplot', dependencies=TRUE)
        library('corrplot')
    }

    if (all(is.na(diag(x))))
        diag(x) = 1

    corrplot(x, ...)

}

# plots

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
    filename = strf('%s%s.%s.png', PLOT_DIR, name, fname)
    png(file=filename, width=960, height=960)
    f(...)
    dev.off()
    print(strf('Saved at %s', filename))
}

save.boxplot = function (...) {
    "Create a boxplot and save the output in a png file."
    save.png(boxplot, ...)
}

save.plot = function (...) {
    "Create a plot and save the output in a png file."
    save.png(plot, ...)
}

save.cor.plot = function (...) {
    "Create a cor.plot and save the output in a png file."
    save.png(cor.plot, ...)
}

save.clusplot = function (...) {
    "Create a clusplot and save the output in a png file."
    if (!require('cluster'))
        library('cluster')
    save.png(clusplot, ...)
}

save.scatterplot3d = function (...) {
    "Create a scatterplot3d and save the output in a png file."
    if (!require('scatterplot3d'))
        library('scatterplot3d')
    save.png(scatterplot3d, ...)
}

chist = function (x, y, palette=rainbow) {
    "Discriminate a dataset x by y to draw cobinated histograms plots.

    References:
    http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
    "
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

mhist = function (x, y, palette=rainbow) {
    "Discriminate a dataset x by y to draw multiple histogram plots.
    "
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

# k-means functions

betweenss.rate = function (fit) {
    "Between-cluster Sum of Square Error rate of a k-means fit. It represents
    the total SSE rate minimized from the data after partitioning in k clusters.

    Same as:
        totss = sum(ss(data))
        tot.withinss = sum(k-means(data, k)$withinss)
        betweenss = tot.withinss - totss
        betweenss.rate = betweenss / totss
    "
    return(fit$betweenss / fit$totss)
}

# deprecated

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
