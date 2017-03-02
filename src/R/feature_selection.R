# Provide feature selection functions to quantify the discriminative power of features.
# In order to perform feature selection, a number of different measures are used in order to quantify the relevance of
# a feature (its discriminative power) to the classification process.
#
# References:
# - Data Classification Algorithms and Applications (2015)
# - COMPARISON OF FILTER BASED FEATURE SELECTION ALGORITHMS: AN OVERVIEW
# - https://pdfs.semanticscholar.org/8adc/91eb8713fdef1ac035d2832990457eec4868.pdf

import_package('FSelector', attach, attach=TRUE)
import('utils', attach=c('correlation_analysis'))

#' Select the features of a matrix x such that min >= f(x) =< max optional thresholds
filter_features = function (x, f, min=NULL, max=NULL) {
    y = apply(x, 2, f)

    select = names(y)
    if (is.null(min) && !is.null(max))
        select = y <= max
    else if (!is.null(min) && is.null(max))
        select = y >= min
    else
        select = ((y >= min) + (y <= max)) == 2

    features = names(y)[select]

    if (length(features) > 0)
        return(features)
    else
        return(NULL)
}

#' Redundant features of a matrix that are equal or grater than correlation threshold
redundant_features = function (data, correlation_threshold=0.65, redundant_features_=NULL) {
    # absolute correlation matrix
    correlation_matrix = render_plot(function () abs(correlation_analysis(data)$estimates), save=FALSE, close=TRUE)

    # highly correlated by feature (correation vector sum | correation >= correlation_threshold)
    highly_correlated_sum = apply(correlation_matrix, 1, function(row) {
        return(sum(row[row >= correlation_threshold]))
    })

    if (is.null(highly_correlated_sum)) {
        return(NULL)
    } else if (length(highly_correlated_sum[highly_correlated_sum > 0]) > 1) {
        most_redundant = names(sort(highly_correlated_sum, decreasing=TRUE)[1])
        redundant_features_ = c(redundant_features_, most_redundant)
        features = setdiff(colnames(data), most_redundant)
        return(redundant_features(data[, features], correlation_threshold, redundant_features_))
    } else {
        return(redundant_features_)
    }
}

#' Function to automatize feature engeneering: redundant_features, zero_variance_features, feature_selection
#'
#' TODO: Improve
feature_engeneering = function (data, features, target, correlation_threshold=0.65,
                                feature_selector=information_gain_selector) {
    # remove redundant features
    features = setdiff(features, redundant_features(data[, features], correlation_threshold=correlation_threshold))

    # remove zero variance features, ie, features with a single 'class'
    zero_variance_features = filter_features(data[, features], function(y) {
        return(if (is.integer(y)) length(unique(y)) else NULL)
    }, max=1)
    features = setdiff(features, zero_variance_features)

    # select features using a feature selector
    features = feature_selector(data, features, target)$features

    return(features)
}

#' Given a dataset, compute feature scores with a method handler and select features based on a score filter
#'
#' References:
#' http://ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#'
#' @seealso cluster_information_gain, gini, relieff
feature_selection = function (data, features, target, method_handler, score_filter) {
    data = if (!is.data.frame(data)) as.data.frame(data) else data
    features = sort(features)

    data = data[, c(features, target)]

    scores = method_handler(data)
    scores = scores[order(scores$attr_importance, decreasing=TRUE), , drop=FALSE]

    # select features by applying score filter on computed scores
    is_selected = score_filter(scores)
    features = sort(rownames(is_selected)[is_selected])

    return(list(scores=scores, features=features))
}

#' Perform feature selection using information gain method for a given data set and target feature.
#'
#' References:
#' http://stackoverflow.com/questions/33241638/use-of-formula-in-information-gain-in-r
#' http://stackoverflow.com/questions/1859554/what-is-entropy-and-information-gain
information_gain_selector = function (data, features, target, score_filter=function (x) x > 0) {
    return(feature_selection(data, features, target, function (data) {
        FSelector::information.gain(as.formula(strf('%s ~ .', target)), data)
    }, score_filter))
}

#' Perform feature selection using gini index method. Compute the gini index for a given data set and binary target.
#' The scores range from 0 to 1 with closer to zero scores assigned to important features.
#'
#' References:
#' https://www.r-bloggers.com/calculating-a-gini-coefficients-for-a-number-of-locales-at-once-in-r/
#' http://stats.stackexchange.com/questions/95839/gini-decrease-and-gini-impurity-of-children-nodes
#' https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
gini_index_selector = function (data, features, target, score_filter=function (x) x < 0.7) {
    # calculate a gini index for a dataset x and multiply by a given proportion p
    gini_index = function (x, proportion=1) {
        attr_importance = as.data.frame(apply(x, 2, ineq::Gini)) * proportion
        return(attr_importance)
    }

    # compute scores given a binary target feature
    return(feature_selection(data, features, target, function (data) {
        data_target_0 = data[data[, target] == 0, ]
        data_target_1 = data[data[, target] == 1, ]

        score_target_0 = gini_index(data_target_0[, features], (nrow(data_target_0) / nrow(data)))
        score_target_1 = gini_index(data_target_1[, features], (nrow(data_target_1) / nrow(data)))
        score = score_target_0 + score_target_1
        colnames(score) = c('attr_importance')
        return(score)
    }, score_filter))
}

#' Perform feature selection using ReliefF method. Compute the ReliefF for a given data set and target. The scores
# range from -1 to 1 with large positive scores assigned to important features.
#'
#' References:
#' ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#' https://www.mathworks.com/help/stats/relieff.html?requestedDomain=www.mathworks.com
relieff_selector = function (data, features, target, score_filter=function (x) x > 0) {
    return(feature_selection(data, features, target, function (data) {
        FSelector::relief(as.formula(strf('%s ~ .', target)), data)
    }, score_filter))
}

#' Perform feature selection using random forest. Compute the feature relevance using random forest for a given data
#' set and target.
#'
#' References:
#' ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#'
#' TODO Review:
#' http://stats.stackexchange.com/questions/56092/feature-selection-packages-in-r-which-do-both-regression-and-classification
#' https://cran.r-project.org/web/packages/varSelRF/varSelRF.pdf
#' http://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
random_forest_selector = function (data, features, target, score_filter=function (x) x > apply(x, 1, mean)) {
    return(feature_selection(data, features, target, function (data) {
        FSelector::random.forest.importance(as.formula(strf('%s ~ .', target)), data)
    }, score_filter))
}
