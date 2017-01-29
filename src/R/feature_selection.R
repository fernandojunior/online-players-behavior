import('utils', attach=c('correlation_analysis'))
import_package('FSelector', attach, attach=TRUE)

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

#' List with redundant features of a data matrix
redundant_features = function (data, redundats_=NULL) {
    correlation_matrix = correlation_analysis(data)$estimates

    # highly correlated sum (>= 0.7) by feature
    highly_correlated_sum = apply(correlation_matrix, 1, function(row) sum(row[abs(row) >= 0.7]))

    if (is.null(highly_correlated_sum)) {
        return(NULL)
    } else if (length(highly_correlated_sum[highly_correlated_sum > 0]) > 1) {
        most_redundant = names(sort(highly_correlated_sum, decreasing=TRUE)[1])
        redundats_ = c(redundats_, most_redundant)
        features = setdiff(colnames(data), most_redundant)
        return(redundant_features(data[, features], redundats_))
    } else {
        return(redundats_)
    }
}

#' Compute feature scores with a feature selection method for a given data set to select features based on a criteria
#' handler
#'
#' References:
#' http://ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#'
#' @seealso information_gain, gini, relieff
feature_selection = function (data, target, method_handler, criteria_handler) {
    data = if (!is.data.frame(data)) as.data.frame(data) else data
    features = sort(colnames(data))

    scores = method_handler(data)
    scores = scores[order(scores$attr_importance, decreasing=TRUE), , drop=FALSE]

    # select features by appplying criteria handler on computed scores
    is_selected = criteria_handler(scores)
    features = rownames(is_selected)[is_selected]

    return(list(scores=scores, features=features))
}

#' Compute a feature selection score matrix (feature_selection_handler) for a given data set for each cluster given a target
#'
#' References:
#' http://ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#'
#' @seealso information_gain, gini, relieff
cluster_feature_selection = function (data, features, target, cluster, scores_handler, criteria_handler=NULL, remove_redundant=TRUE) {
    data = as.data.frame(data)
    features = sort(features)
    clusters = sort(unique(data[, cluster]))
    cluster_names = lapply(clusters, function (i) strf('%s%s', cluster, i))

    # matrix to collect the scores (information gain) for each feature x cluster
    score_matrix = matrix(0, nrow=length(clusters), ncol=length(features))
    dimnames(score_matrix) <- list(cluster_names, features)

    # compute scores for each cluster given a binary target feature
    for(i in clusters) {
        cluster_data = data[data[, cluster] == i, c(features, target)]
        score = scores_handler(cluster_data)
        score_matrix[strf('%s%s', cluster, i), ] = round(score[order(rownames(score)), ], digits=3)
    }

    result = list(score=score_matrix)

    if (!is.null(criteria_handler)) {
        is_selected = criteria_handler(score_matrix)

        result$selection = apply(is_selected, 1, function (row) {
            return(colnames(is_selected)[row])
        })

        result$redundant = lapply(clusters, function (i) {
            cluster_data = data[data[, cluster] == i, c(result$selection[[strf('%s%s', cluster, i)]] )]
            redundant_features(cluster_data)
        })
        names(result$redundant) = cluster_names

        if (remove_redundant == TRUE) {
            result$selection = lapply(cluster_names, function (i) {
                return(setdiff(values(result$selection[[i]]), result$redundant[[i]]))
            })
            names(result$selection) = cluster_names
        }
    }

    return(result)
}

#' Compute the information gain for a given data set for each label given a target
#' References:
#' http://stackoverflow.com/questions/33241638/use-of-formula-in-information-gain-in-r
#' http://stackoverflow.com/questions/1859554/what-is-entropy-and-information-gain
information_gain = function (data, features, target, label, criteria_handler=NULL) {
    return(cluster_feature_selection(data, features, target, label, function (cluster) {
        FSelector::information.gain(as.formula(strf('%s ~ .', target)), cluster)
    }, criteria_handler))
}

#' Compute the gini index for a given data set for each label given a binary target
#' References:
#' https://www.r-bloggers.com/calculating-a-gini-coefficients-for-a-number-of-locales-at-once-in-r/
#' http://stats.stackexchange.com/questions/95839/gini-decrease-and-gini-impurity-of-children-nodes
#' https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
gini = function (data, features, target, label, criteria_handler=NULL) {
    # calculate a gini index for a data matrix x and multiply by a given proportion p
    gini_ = function (x, p=1) {
        return(as.data.frame(apply(x, 2, ineq::Gini)) * p)
    }

    # compute scores for each cluster given a binary target feature
    return(cluster_feature_selection(data, features, target, label, function (cluster) {
        cluster_target_0 = cluster[cluster[, target] == 0, ]
        cluster_target_1 = cluster[cluster[, target] == 1, ]

        score_target_0 = gini_(cluster_target_0[, features], (nrow(cluster_target_0) / nrow(cluster)))
        score_target_1 = gini_(cluster_target_1[, features], (nrow(cluster_target_1) / nrow(cluster)))
        score = score_target_0 + score_target_1
        return(score)
    }, criteria_handler))
}

#' Compute the ReliefF for a given data set for each label given a target. The weights range from -1 to 1 with large
#' positive weights assigned to important features.
#'
#' References:
#' ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
#' https://www.mathworks.com/help/stats/relieff.html?requestedDomain=www.mathworks.com
relieff = function (data, features, target, label, criteria_handler=NULL) {
    return(cluster_feature_selection(data, features, target, label, function (cluster) {
        FSelector::relief(as.formula(strf('%s ~ .', target)), cluster)
    }, criteria_handler))
}

#' Compute the feature relevance using random forest for a given data set for each label given a target
#' References:
#' ijirts.org/volume2issue2/IJIRTSV2I2034.pdf
random.forest.importance = function (data, features, target, label, criteria_handler=NULL) {
    return(cluster_feature_selection(data, features, target, label, function (cluster) {
        FSelector::random.forest.importance(as.formula(strf('%s ~ .', target)), cluster)
    }, criteria_handler))
}
