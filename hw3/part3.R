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
boxplot(death ~ bymedian,
        ylab = '30-Day Death Rate',
        las = 2)
title("Heart Attack 30-day Death Rate by State")
