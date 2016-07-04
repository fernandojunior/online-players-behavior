# Functions to help in correlation analysis

# library('modules')
import('fun', attach=c('or'))

#' Test for correlation `cor.test` between paired columns of a matrix x.
#'
#' @param x Numeric matrix of data value.
#' @param method {character optional 'pearson'} Character string indicating
#'     which correlation coefficient is to be used for the test. One of
#'     'pearson', 'kendall', or 'spearman'.
#' @param ... Arguments to pass in cor.test
#'
#' @return Correlations (estimates); p-values;  conf. intervals
#'
#' @examples
#'     a = c(462, 842, 912, 531, 215, 526, 140, 673, 610, 309)
#'     b = c(21, 493, 549, 684, 401, 133, 433, 758, 567, 253)
#'     c = a + b
#'     x = cbind(a, b, c)
#'     result = correlation_matrix(x)
#'     result$estimates
#'     #>           a         b         c
#'     #> a        NA 0.3730823 0.8420527
#'     #> b 0.3730823        NA 0.8146049
#'     #> c 0.8420527 0.8146049        NA
#'     result$p.values
#'     #>             a           b           c
#'     #> a 0.000000000 0.288305694 0.002239982
#'     #> b 0.288305694 0.000000000 0.004105174
#'     #> c 0.002239982 0.004105174 0.000000000
#'     result$conf.int
#'     #> $conf.int$max
#'     #>           a         b         c
#'     #> a 1.0000000 0.8119740 0.9617695
#'     #> b 0.8119740 1.0000000 0.9546133
#'     #> c 0.9617695 0.9546133 1.0000000
#'     #> $conf.int$min
#'     #>            a          b         c
#'     #> a  1.0000000 -0.3353088 0.4521429
#'     #> b -0.3353088  1.0000000 0.3797523
#'     #> c  0.4521429  0.3797523 1.0000000
#'
#' @seealso cor.test
correlation_matrix = function(x, method='pearson', ...) {
    cols = or(colnames(x), 1:ncol(x))
    n = length(cols)

    basematrix = matrix(NA, n, n)
    colnames(basematrix) = rownames(basematrix) = cols

    r = list(estimates=basematrix, p.values=basematrix)  # result

    # A correlation of the diagonal correlation matrix is a correlation of a
    # data attribute with itself. So, the diagonal must be NA to prevent wrong
    # behaviors as in dendrogram and box plots.
    diag(r$estimates) = NA

    diag(r$p.values) = 0

    if (method == 'pearson') {
        r$conf.int = list()
        r$conf.int$min = r$conf.int$max = basematrix
        diag(r$conf.int$min) = diag(r$conf.int$max) = 1
    }

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp = cor.test(x[, i], x[, j], method=method, ...)
            r$estimates[i, j] = r$estimates[j, i] = tmp$estimate
            r$p.values[i, j] = r$p.values[j, i] = tmp$p.value
            if ('conf.int' %in% names(tmp)) {
                r$conf.int$min[i, j] = r$conf.int$min[j, i] = tmp$conf.int[1]
                r$conf.int$max[i, j] = r$conf.int$max[j, i] = tmp$conf.int[2]
            }
        }
    }

    return(r)
}
