rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
  titlecase <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=".")
  }

  # colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.'
  colname <- paste(colname, titlecase(outcome), sep='')
  caredata <- read.csv('outcome-of-care-measures.csv',
                       na.strings='Not Available')

  if(!(state %in% caredata$State)) stop('invalid state')
  if(!(colname %in% colnames(caredata))) stop('invalid outcome')

  x <- caredata[caredata$State == state, c('Hospital.Name', colname)]
  # x <- transform(x, as.numeric(colname))
  ## x[x[,colname] == 'Not Available',colname] <- NA
  ## x <- na.omit(x)
  ii <- order(x[2], x[1], na.last=NA, decreasing=FALSE)
  nitems <- length(ii)
  if(num == 'best') {
    num <- 1
  }
  else if(num == 'worst') {
    num <- nitems
  }
  
  if(num > nitems) {
    return(NA)
  }

  x[[ii[num],1]]
}

