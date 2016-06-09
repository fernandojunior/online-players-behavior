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

features = c('firstBloodKill',
'firstInhibitorKill', 'firstTowerKill', 'firstInhibitorAssist', 'firstTowerAssist')

summarized = aggregate(. ~ matchId + winner, data[, c('matchId', 'winner', features)], sum)

par(mfrow=c(2, 3))
for (feature in features) {
    x = summarized[, feature]
    y = summarized[, 'winner']
    palette = rainbow
    counts = counter_by(x, y)
    rownames(counts) = c('false', 'true')

    # http://yatani.jp/teaching/doku.php?id=hcistats:chisquare
    test = chisq.test(counts)
    rate = abs(apply(counts, 2, diff))/apply(counts, 2, sum)
    legends = map(function (item) {
        strf('%s; %s%%', item, (100 * round(rate[indexof(item, colnames(counts))], 2)))
    }, colnames(counts))

    main = strf('X = %s', feature)
    xlab = 'Y = winner; domain(Y) = {false, true}'
    ylab = 'C = counts(distinct:X, by:Y)'
    ylim = c(0, sum(counts[1, ]))
    print(ylim)
    colors = adjustcolor(palette(length(colnames(counts))), alpha.f = 0.3)
    plt = barplot(t(counts), col=colors, main=main, beside=TRUE, ylim=ylim, ylab=ylab, xlab=xlab)
    text(plt, t(counts) + 300, labels=t(counts), col="black", srt=60, cex=0.5)
    legend("topright", legend=legends, col=colors, lwd = 5, title='domain(X); abs(rate(C))', cex=0.5)
}
