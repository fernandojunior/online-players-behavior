# Functions to handle outliers

import('fun', attach=c('or', 'Col', 'Curry', 'values', 'Row', 'strf', 'interval'))

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

    # select only features where the max and min thresholds are different
    # lower != upper in order to mantain the variability
    cols = diff(lim) != 0
    lim[cols]
    x = x[, cols]

    # indicate which element is an outlier TRUE or not FALSE
    is_outlier = function (e) !interval(e, lim['lower', ], lim['upper', ])
    outliers = Row(is_outlier, x)
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
