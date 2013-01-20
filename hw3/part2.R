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
  hist(x[, colidx],
       main = paste(sub('\\.', ' ', titlecase(outcomes[i]), "30-Day Death Rate")),
       xlab = "30-Day Death Rate")
}
