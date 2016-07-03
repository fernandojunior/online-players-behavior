# helper functions

import_package('corrplot', attach=TRUE)  # corrplot

#' @aliases sprintf
strf = function (...) {
    return(sprintf(...))
}

#' @aliases table
counter = function (...) {
    return(table(...))
}

#' @aliases match
indexof = function (e, l) {
    return(match(e, l))
}

#' @deprecated Cuz override the built-in funtion range.
#'
#' @aliases seq.int
#'
#' @examples
#'     range(10)
#'     #> [1]  1  2  3  4  5  6  7  8  9 10
#'     range(5,10)
#'     #> [1]  5  6  7  8  9 10
#'     range(0, 10, 2)
#'     #> [1]  0  2  4  6  8 10
range = function (...) {
    return(seq.int(...))
}

#' Apply a function f on each element in x and return the results.
#'
#' Curring: map(f, x) == map(f)(x)
#'
#' @param f Function to apply.
#' @param x Vector or matrix.
#'
#' @return The results.
#'
#' @examples
#'     map(function (a) a + 1, 1)
#'     #> [1] 2
#'     map(function (a) a + 1, c(1,2,3))
#'     #> [1] 2 3 4
#'     map(function (a) a + 1, rbind(c(1,2), c(1,2)))
#'     #>     [,1] [,2]
#'     #> [1,]    2    3
#'     #> [2,]    2    3
map = function (f, x) {
    if (missing(x))
        return(Curry(map, f))
    if (is.list(x))
        return(lapply(x, f))
    else if (is.vector(x))
        return(sapply(x, f))
    else if (is.matrix(x) | is.data.frame(x))
        return(apply(x, 2, function (y) map(f, y)))
}

#' Apply a function f on each column of x and return the results.
#'
#' Curring: Col(f, x) == Col(f)(x)
#'
#' @param f Function to apply.
#' @param x Matrix.
#'
#' @return The results.
#'
#' @examples
#'     Col(sum, rbind(c(1,2), c(3,4)))
#'     #> [1] 4 6
Col = function (f, x) {
    if (missing(x))
        return(Curry(Col, f))
    return(apply(x, 2, f))
}

#' Apply a function f on each row of x and return the results.
#'
#' Curring: Row(f, x) == Row(f)(x)
#'
#' @param f Function to apply.
#' @param x Matrix.
#'
#' @return The results.
#'
#' @examples
#'     Row(sum, rbind(c(1,2), c(3,4)))
#'     #> [1] 3 7
Row = function (f, x) {
    if (missing(x))
        return(Curry(Row, f))
    return(apply(x, 1, f))
}

#' Prespecify arguments for a function f to create and return a new one.
#'
#' @param f Function to be curried.
#' @param ... Arguments.
#'
#' @return Function f with prespecified arguments.
#'
#' @examples
#'     Curry(paste, collapse='')(c(1, 2, 3))
#'     # [1] "123"
#'
#' @references
#'     stackoverflow.com: higher level functions in R - is there an official
#'     compose operator or curry function?
Curry = function(f, ...) {
    args = list(...);
    return(function(...) do.call(f, c(args,list(...))))
}

#' Compose an arbitrary number of functions to create and return a new one.
#'
#' @param ... Functions to be composed.
#'
#' @return A composite function.
#'
#' @examples
#'     Compose(sum, sqrt)(c(1,2,3))
#'     #> [1] 2.44949
#'     x = c(a=1,b=2,c=3)
#'     x = Compose(print, values, print, sum, print, sqrt, print)(x)
#'     #> a b c
#'     #> 1 2 3
#'     #> [1] 1 2 3
#'     #> [1] 6
#'     #> [1] 2.44949
#'     x
#'     #> [1] 2.44949
#'     Compose(each(print), Col(sum))(rbind(c(1,2), c(3,4)))
#'     #> [1] 1
#'     #> [1] 3
#'     #> [1] 2
#'     #> [1] 4
#'     #> [1] 4 6
#'
#' @references
#'     stackoverflow.com: higher level functions in R - is there an official
#'     compose operator or curry function?
Compose = function(...) {
    args = list(...)
    return(function(...) Reduce(function(x, f) f(x), args, ...))
}

#' Simplify x by removing its names attribute.
#'
#' @param x Vector or matrix.
#'
#' @return Simplified vector or matrix.
#'
#' @examples
#'     values(list(a=1, b=2))
#'     #> [1] 1 2
#'     values(rbind(c(a=1, b=2), c(c=3, c=4)))
#'     #>       [,1] [,2]
#'     #> [1,]    1    2
#'     #> [2,]    3    4
values = function (x) {
    if (is.list(x) && !is.matrix(x))
        x = unlist(x)
    if (is.data.frame(x))
        x = data.matrix(x)
    return(unname(x))
}

#' Iterate over the elements of x and execute a function f on each one.
#'
#' Curring: each(f, x) == each(f)(x)
#'
#' @param x Vector
#'
#' @return x
#'
#' @examples
#'     x = c(3, 4, 5)
#'     each(print, x)
#'     #> [1] 3
#'     #> [1] 4
#'     #> [1] 5
#'     #> [1] 3 4 5
#'     each(function (x, i) print(paste(i, x + 1)), x)
#'     #> [1] "1 4"
#'     #> [1] "2 5"
#'     #> [1] "3 6"
#'     #> [1] 3 4 5
each = function (f, x) {
    if (missing(x))
        return(Curry(each, f))
    indices = or(names(x), range(length(x)))
    if (length(or(formals(f), 1)) == 1)
        for (i in indices)
            f(x[i])
    else
        for (i in indices)
            f(x[i], i)
    return(x)
}

#' Iterate over the columns of x and execute a function f on each one.
#'
#' Curring: each_col(f, x) == each_col(f)(x)
#'
#' @return x
#'
#' @examples
#'     x = cbind(a=c(1, 2), b=c(3, 4))
#'     each_col(function (c) print(sum(c)), x)
#'     #> [1] 3
#'     #> [1] 7
#'     #>      a b
#'     #> [1,] 1 3
#'     #> [2,] 2 4
#'     each_col(function(c, i) print(paste(i, sum(c))), x)
#'     #> [1] "a 3"
#'     #> [1] "b 7"
#'     #>      a b
#'     #> [1,] 1 3
#'     #> [2,] 2 4
each_col = function (f, x) {
    if (missing(x))
        return(Curry(each_col, f))
    indices = or(names(x), colnames(x), range(ncol(x)))
    if (length(or(formals(f), 1)) == 1)
        xxx = Col(f, x)
    else
        each(function (i) f(x[, i], i), indices)
    return(x)
}

#' Iterate over the rows of x and execute a function f on each one.
#'
#' Curring: each_row(f, x) == each_row(f)(x)
#'
#' @return x
#'
#' @examples
#'     x = rbind(a=c(1, 2), b=c(3, 4))
#'     each_row(function(r) print(sum(r)), x)
#'     #> [1] 3
#'     #> [1] 7
#'     #>   [,1]  [,2]
#'     #> a    1    2
#'     #> b    3    4
#'     each_row(function(r, i) print(paste(i, sum(r))), x)
#'     #> [1] "a 3"
#'     #> [1] "b 7"
#'     #> [,1] [,2]
#'     #> a    1    2
#'     #> b    3    4
each_row = function (f, x) {
    if (missing(x))
        return(Curry(each_row, f))
    if (length(or(formals(f), 1)) == 1)
        xxx = Row(f, x)
    else
        each(function (i) f(x[i, ], i), or(rownames(x), range(nrow(x))))
    return(x)
}

#' Apply the logical operator OR on the arguments.
#'
#' Similar to `any`
#'
#' @examples
#'     or(TRUE, TRUE)
#'     #> [1] TRUE
#'     or(FALSE, TRUE)
#'     #> [1] TRUE
#'     or(TRUE, FALSE)
#'     #> [1] TRUE
#'     or(FALSE, (3 == 4))
#'     #> [1] FALSE
#'     or('Cat', 'Dog')
#'     #> [1] 'Cat'
#'     or(FALSE, 'Cat')
#'     #> [1] 'Cat'
#'     or('Cat', FALSE)
#'     #> [1] 'Cat'
#'     or(c(FALSE, TRUE), 'Dog')
#'     #> [1] 'Dog'
#'     or(c(TRUE, TRUE), FALSE)
#'     #> [1] TRUE TRUE
#'     or(NULL, FALSE, 'Dog', 'Cat')
#'     #> [1] 'Dog'
#'     or(TRUE, NULL, 'Dog')
#'     #> [1] TRUE
#'     or(FALSE, NULL, 'Dog')
#'     #> [1] 'Dog'
or = function (...) {
    for (i in list(...))
        if (!is.null(i) && (!is.logical(i) || is.logical(i) && all(i)))
            return(i)
    return(FALSE)
}

#' Apply the logical operator AND on the arguments.
#'
#' Similar to `all`
#'
#' @examples
#'     and(TRUE, TRUE)
#'     #> [1] TRUE
#'     and(TRUE, FALSE)
#'     #> [1] FALSE
#'     and(FALSE, TRUE)
#'     #> [1] FALSE
#'     and(FALSE, (3 == 4))
#'     #> [1] FALSE
#'     and('Cat', 'Dog')
#'     #> [1] 'Dog'
#'     and(FALSE, 'Cat')
#'     #> [1] FALSE
#'     and('Cat', FALSE)
#'     #> [1] FALSE
#'     and(c(FALSE, TRUE), 'Dog')
#'     #> [1] FALSE, TRUE
#'     and(c(TRUE, TRUE), FALSE)
#'     #> [1] FALSE
#'     and(NULL, FALSE, 'Dog', 'Cat')
#'     #> [1] NULL
#'     and(TRUE, NULL, 'Dog')
#'     #> [1] NULL
#'     and(FALSE, NULL, 'Dog')
#'     #> [1] FALSE
and = function (...) {
    args = list(...)

    for (i in range(length(args))) {
        value = args[[i]]
        if (length(args[[i]]) > 1)
            value = do.call(and, as.list(args[[i]]))
        if (is.null(value))
            return(args[[i]])
        if (value == FALSE)
            return(args[[i]])
    }

    for (i in range(2, length(args)))
        if (!identical(args[i - 1], args[i]))
            return(args[[length(args)]])

    return(args[[1]])
}

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

#' Perform a correlation analysis on a matrix x.
#'
#' @references
#'     https://rpubs.com/gaston/dendrograms
#'     https://cran.r-project.org/web/packages/corrplot
correlation_analysis = function (x) {
    # Correlation matrix of x using Spearman method, which does not require the
    # features follow a normal distribuition or linear correlation.
    correlations = cor.mtest(x, method='spearman', exact=FALSE)

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

# ourlier functions

#' Find the lower and upper outlier thresholds of x.
#'
#' Any point greater than upper threshold or less than lower threshold is
#' considered an outlier.
#'
#' A factor is used to determine the upper and lower threshold. The default
#' factor 1.5 (based on Turkey boxplot) indicates that the minimum and
#' maximum range of a point is 50% less and greater than IQR, respectively.
#'
#' If x is a matrix or data frame, find the thresholds on each column.
#'
#' @examples
#'     outlier_thresholds(1:100)
#'     #> lower upper
#'     #> -48.5 149.5
#'     a = c(462, 842, 912, 531, 215, 526, 140, 673, 610, 309)
#'     b = c(21, 493, 549, 684, 401, 133, 433, 758, 567, 253)
#'     x = cbind(a, b)
#'     outlier_thresholds(x)
#'     #> a       b
#'     #> lower -117.75 -118.75
#'     #> upper 1122.25  971.25
outlier_thresholds = function (x, factor=1.5) {
    if (is.matrix(x) || is.data.frame(x))
        return(Col(Curry(outlier_thresholds, factor=factor), x))
    quartiles = values(quantile(x)[2:4])
    iqr = quartiles[3] - quartiles[1] # same as IQR(x)
    lower = quartiles[1] - (iqr * factor)
    upper = (iqr * factor) + quartiles[3]
    threshold = c(lower=lower, upper=upper)
    return(threshold)
}

#' Verify if x is an outlier based on lower and upper thresholds.
#'
#' If x is multivariate (length(x) > 1), so the thresholds also must be.
#'
#' @examples
#'     is.outlier(4, 1, 5)
#'     #> [1] FALSE
#'     is.outlier(0, 1, 5)
#'     #> [1] TRUE
#'     is.outlier(c(1, 2, 3), c(0, 0, 0), c(1, 2, 3))
#'     #> [1] FALSE
#'     is.outlier(x=c(1, 2, 3), lower=c(2, 0, 0), upper=c(1, 2, 3))
#'     #> [1] TRUE  # x[1] < lower[1]
#'     is.outlier(c(1, 2, 3), lower=c(0, 0, 0), upper=c(1, 2, 2))
#'     #> [1] TRUE  # x[3] > lower[3]
is.outlier = function (x, lower, upper) {
    thresholds = rbind(lower, upper)
    if(!is.null(colnames(thresholds)))
        x = x[colnames(thresholds)]
    if (any(x < thresholds['lower', ]) | any(x > thresholds['upper', ]))
        return(TRUE)
    return(FALSE)
}

#' Find outliers on a matrix x based on boxplot IQR factor.
#'
#' @param x Matrix
#' @param factor {numeric optional 1.5} Boxplot IQR factor
#'
#' @todo is.vector(x) == TRUE || is.matrix(x)
#'
#' @return Vector indicating the outliers; thresholds for each column; total
#'     outliers.
#'
#' @examples
#'     a = c(462, 842, 912, 531, 215, 526, 140, 673, 610, 309)
#'     b = c(21, 493, 549, 684, 401, 133, 433, 758, 567, 253)
#'     x = cbind(a, b)
#'     result = find_outliers(x, factor=0.25)
#'     result$outliers
#'     #> [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#'     result$thresholds
#'     #>            a       b
#'     #> lower 269.75 221.875
#'     #> upper 734.75 630.625
#'     result$total
#'     #> [1] 8
find_outliers = function (x, factor=1.5) {
    lim = outlier_thresholds(x, factor=factor)
    cols = or(colnames(lim), 1:ncol(lim))

    # select only features where the max and min thresholds are different
    # lower != upper in order to mantain the variability
    lim = lim[, cols[diff(lim) != 0]]

    # indicate which element is an outlier TRUE or not FALSE
    outliers = Row(function(e) is.outlier(e, lim['lower', ], lim['upper', ]), x)
    total = sum(outliers)

    return(list(outliers=outliers, thresholds=lim, total=total))
}

#' Remove outliers from a matrix x based on boxplot IQR factor.
#'
#' @param x Matrix
#' @param cols {character optional} Predefined matrix columns to find outliers
#' @param factor {numeric optional 1.5} Boxplot IQR factor
#'
#' @todo is.vector(x) == TRUE || is.matrix(x)
#'
#' @return x without outliers
#'
#' @examples
#'     a = c(462, 842, 912, 531, 215, 526, 140, 673, 610, 309)
#'     b = c(21, 493, 549, 684, 401, 133, 433, 758, 567, 253)
#'     x = cbind(a, b)
#'     result = find_outliers(x, factor=0.25)
#'     result$outliers
#'     #> [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#'     result$thresholds
#'     #>            a       b
#'     #> lower 269.75 221.875
#'     #> upper 734.75 630.625
#'     result$total
#'     #> [1] 8
remove_outliers = function (x, cols=NULL, factor=1.5) {
    cols = or(cols, colnames(x), 1:ncol(x))
    result = find_outliers(x[, cols], factor=factor)
    print('Thresholds:')
    print(t(result$thresholds))
    print('Total outliers:')
    print(t(result$total))
    return(x[!result$outliers, ])
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

# correlation functions

#' Count the number of items from a list or vector of correlations x.
#'
#' If x is a matrix or data frame, count the number of items for each column.
cor.counter = function (x) {
    if (is.matrix(x) | is.data.frame(x))
        return(apply(x, 2, cor.counter))
    return(counter(x))
}

#' Mean of a list or vector of correlations x.
#'
#' If x is a matrix or data frame, calculate the mean for each column.
cor.mean = function (x) {
    if (is.matrix(x) | is.data.frame(x))
        return(apply(x, 2, cor.mean))
    return(mean(x[!x %in% NA]))
}

#' Correlation rank of the attributes of a correlation matrix.
#'
#' It is based on the mean of correlations for each one.
cor.rank = function (correlation_matrix, decreasing=TRUE) {
    return(names(sort(cor.mean(correlation_matrix), decreasing=decreasing)))
}

#' Peform a cor.test between paired features of a multivariate data set x.
cor.mtest = function(x, method='pearson', ...) {
    features = colnames(x)
    n = length(features)

    basematrix = matrix(NA, n, n)
    colnames(basematrix) = rownames(basematrix) = features

    r = list()
    r$estimates = r$p.values = basematrix

    # A correlation of the diagonal correlation matrix is a correlation of a
    # data attribute with itself. So, the diagonal must be NA to prevent wrong
    # behaviors as in dendrogram and box plots.
    diag(r$estimates) = NA

    diag(r$p.values) = 0

    if (method == 'pearson') {
        r$conf.int = list()
        r$conf.int$lower = r$conf.int$upper = basematrix
        diag(r$conf.int$lower) = diag(r$conf.int$upper) = 1
    }

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp = cor.test(x[, i], x[, j], method=method, ...)
            r$estimates[i, j] = r$estimates[j, i] = tmp$estimate
            r$p.values[i, j] = r$p.values[j, i] = tmp$p.value
            if ('conf.int' %in% names(tmp)) {
                r$conf.int$lower[i, j] = r$conf.int$lower[j, i] = tmp$conf.int[1]
                r$conf.int$upper[i, j] = r$conf.int$upper[j, i] = tmp$conf.int[2]
            }
        }
    }

    return(r)
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

# k-means functions

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

# deprecated

#' Automatic attribute selection of a correlation matrix.
#'
#' It is based on the non-correlations counter and mean of correlations for
#' each column (attribute) of the correlation matrix.
#'
#' Formalization:
#'     C = C(X) = {correlation matrix of a data X}
#'     A = A(X) = A(C) = {attributes of the correlation matrix C}
#'     c(a) in C = {correlations of attibute a}
#'     select(a) = 0, if qt0(a) > m(qt0(A)) And m(a) < md(m(A)); 1 otherwise
#'         where 0 = False and 1 = True
#'     selection(C) = { a in A(C) | select(a)}
attribute_selection = function (correlation_matrix) {
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
        # Return TRUE if attribute must be selected, FALSE otherwise.
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
