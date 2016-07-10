# Fun functions and aliases for functional programming

#' @aliases
#' @todo axis = c(1, 2)
concat = function (...) {
    return(cbind(...))
}

#' @aliases names
keys = function (...) {
    return(names(...))
}

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
each = function (f, x) {
    if (missing(x))
        return(Curry(each, f))
    for (i in or(names(x), range(length(x))))
        f(x[i])
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
