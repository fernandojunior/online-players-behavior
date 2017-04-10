# Functions to evaluate a predictive model
# http://journals.plos.org/plosone/article/figure/image?size=large&id=info:doi/10.1371/journal.pone.0118432.t001

#           targets
# outcomes  0  1
#        0  TN FN
#        1  FP TP
confusion_matrix = function (outcomes, targets) {
    return(table(outcomes, targets))
}

# the proportion of the total number of predictions that were correct.
accuracy = function (confusion_matrix) {
    return(sum(diag(confusion_matrix))/sum(confusion_matrix))
}

# The proportion of  positive predictive cases that were correctly identified.
# Aliases: positive predictive value
precision = function (tp, fp) {
    pp = (tp + fp)
    return(tp / pp)
}
# taxa de previsoes de times que realmente venceram em relacao ao total de previsoes de times vencedores
# previsão de times vencedores que realmente vencenram
# previsões corretas de times vencedores

# The proportion of actual positive cases which are correctly identified.
# Aiases: sensitivity, true positive rate, probability of detection
recall = function (tp, fn) {
    p = (tp + fn)
    return(tp / p)
}
# taxa de previsoes de times que realmente venceram em relacao ao total de times vencedores
# previsões corretas de times vencedores em relação total de times vencedores

# The proportion of actual negative cases which are correctly identified.
# Alias: true negative rate, fall-out or probability of false alarm
specificity = function (tn, fp) {
    return(tn / (tn + fp))
}

# Alias?
false_positive_rate = function (tn) {
    n = tn + fp
    return(fp / n)
}

# confusion_matrix: outcomes x targets
f_measure = function (confusion_matrix) {
    tp = confusion_matrix[2, 2]
    fp = confusion_matrix[2, 1]
    fn = confusion_matrix[1, 2]
    precision = precision(tp, fp)
    recall = recall(tp, fn)
    return(2 * (precision * recall) / (precision + recall))
}

# Evaluate prediction outcomes
evaluate_outcomes = function (targets, outcomes) {
    confusion_matrix=confusion_matrix(outcomes, targets)
    return(list(
        confusion_matrix=confusion_matrix,
        accuracy=accuracy(confusion_matrix),
        f_measure=f_measure(confusion_matrix)
    ))
}


install.packages('ROCR', dependencies=TRUE)
import_package('ROCR', attach=TRUE)

roc_curve = function (outcomes, targets) {
    # outcomes = as.numeric(outcomes)
    # targets = as.numeric(targets)
    performance = ROCR::performance(prediction(predictions=outcomes, labels=targets) , "tpr", "fpr")
    # changing params for the ROC plot - width, etc
    # par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
    # plotting the ROC curve
    plot(performance,col="black",lty=3, lwd=3)
    # plot(perf,col="black",lty=3, lwd=3)
}
