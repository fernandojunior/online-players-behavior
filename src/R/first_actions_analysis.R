source('utils.R')

options(scipen=999)

# ---------
# Load data
# ---------

# A data set with n = 85470 points/tuples/rows, where a point p represents a
# feature vector of a participant in a specific match. Each match has only 10
# participants.
data = read.csv('../data/data20160609012259.csv')
# > nrow(data)
# [1] 85470

features = c('winner', 'firstBloodKill',
'firstInhibitorKill', 'firstTowerKill', 'firstInhibitorAssist', 'firstTowerAssist')

teams = aggregate(. ~ matchId + winner, data[, c('matchId', features)], sum)
counts = counter_by(. ~ winner, teams[, features])
rownames(counts) = counts[, 1]

par(mfrow=c(1, 5))
for (feature in setdiff(features, 'winner')) {
    palette = rainbow
    counts_ = counts[feature]
    counts_names = colnames(counts_[[1]])
    test = chisq.test(counts_)
    rate = abs(apply(counts_, 2, diff))/apply(counts_, 2, sum)
    legends = map(function (item) {
        strf('%s; %s%%', item, (100 * round(rate[indexof(item, counts_names)], 2)))
    }, counts_names)
    print(counts_)
    main = strf('X = %s', feature)
    xlab = 'Y = winner; domain(Y) = {false, true}'
    ylab = 'C = counts(distinct:X, by:Y)'
    ylim = c(0, sum(counts_[1, ]))
    colors = adjustcolor(palette(length(counts_names)), alpha.f = 0.3)
    plt = barplot(t(counts_), col=colors, main=main, beside=TRUE, ylim=ylim, ylab=ylab, xlab=xlab)
    text(plt, t(counts_) + 300, labels=t(counts_), col="black", srt=60, cex=0.5)
    legend("topright", legend=legends, col=colors, lwd = 5, title='domain(X); abs(rate(C))', cex=0.5)
}
