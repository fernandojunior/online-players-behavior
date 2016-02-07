# ---------
# Constants
# ---------

PLOTS = 'plots/'

# helper functions

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

    If x is a vector or a list, it applies for each item.

    if x is a matrix or data frame, it applies for each item, for each column.

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

range = function (...) {
    "Alias for seq.int. Override the built-in funtion."
    return(seq.int(...))
}

type = function (...) {
    "Alias for typeof."
    return(typeof(...))
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

# string functions

endswith = function (s, suffix) {
    "Return TRUE if s ends with the specified suffix, FALSE otherwise."
    return(grepl(strf('%s$', suffix), s))
}

strf = function (...) {
    "Alias for sprintf."
    return(sprintf(...))
}

startswith = function (s, prefix) {
    "Return TRUE if s starts with the specified prefix, FALSE otherwise."
    return(grepl(strf('^%s', prefix), s))
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

    x point can be multivariate. In this case, the thresholds also must be.
    TODO improve...
    "
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
    filename = strf('%s%s.%s.png', PLOTS, name, fname)
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
