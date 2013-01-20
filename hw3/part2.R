titlecase <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
}

x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomes <- c('heart attack', 'heart failure', 'pneumonia')

par(mfrow = c(3, 1))
for (i in 1:length(outcomes)) {
  colidx <- which(colnames(x) == 
                  paste("Hospital.30.Day.Death..Mortality..Rates.from.",
                        titlecase(outcomes[i]), sep=''))
  x[,colidx] <- as.numeric(x[,colidx])
  title = paste(sub('\\.', ' ', titlecase(outcomes[i])), "30-Day Death Rate")
  ndigit <- 2
  title = paste(title, 'bar(x) =',
    format(round(mean(x[,colidx], na.rm=TRUE), 2), nsmall=2))
  hist(x[, colidx],
       main = title,
       xlab = "30-Day Death Rate",
       prob=TRUE)
  abline(v=mean(x[,colidx], na.rm=TRUE), col="blue", lwd=2)
  lines(density(x[,colidx] , na.rm=TRUE, adjust=2))
}
