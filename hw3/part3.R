outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])

thresh <- 20
x <- table(outcome$State)
x <- x[x > thresh]
outcome2 <- outcome[outcome$State %in% names(x),]

death <- outcome2[, 11]
state <- outcome2$State
## boxplot(death ~ state,
##         ylab = '30-Day Death Rate',
##         las = 2)
## title("Heart Attack 30-day Death Rate by State")

# do.call(rbind, by(x, x$State, rankall_helper))

bymedian <- reorder(outcome2$State, outcome2[,11], function(x) { median(x,na.rm=TRUE)})
z <- x[levels(bymedian)]
fancylabels <- paste(levels(bymedian),"(", z, ")", sep="")
# levels(bymedian) <- fancylabels

## boxplot(death ~ bymedian,
##         ylab = '30-Day Death Rate',
##         las = 2,
##         cex.axis = 0.8,
##         xaxt = 'n')
## title("Heart Attack 30-day Death Rate by State")

boxplot(death ~ bymedian,
        ylab = '30-Day Death Rate',
        las = 2,
        xaxt = 'n')
title("Heart Attack 30-day Death Rate by State")
axis(1, labels=fancylabels, at=1:length(fancylabels), las=2, cex.axis=0.8)
