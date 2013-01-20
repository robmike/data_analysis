rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

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

  if(!(colname %in% colnames(caredata))) stop('invalid outcome')

  x <- caredata[, c('Hospital.Name', colname, 'State')]
  # x <- transform(x, as.numeric(colname))
  ## x[x[,colname] == 'Not Available',colname] <- NA
  ## x <- na.omit(x)

  rankall_helper <- function(x) {
    ii <- order(x[,colname], x$Hospital.Name, na.last=NA, decreasing=FALSE)
    # Return NA hospital name if fewer than num hospitals in state
    if(is.numeric(num) & num > nrow(x)) {       #FIXME: Are always guaranteed one entry?
      out <- x[1,]
      out[,'Hospital.Name'] <- NA
    }
    else {
      nitems <- length(ii)
      if(num == 'best') {
        num <- 1
      }
      else if(num == 'worst') {
        num <- nitems
      }
      out <- x[ii[num],]
      out[,'Hospital.Name'] <- as.character(out[,'Hospital.Name'])
    }
    out <- out[,c('Hospital.Name', 'State')]
    colnames(out) = c('hospital','state')
    out
  }

  # aggregate(x, list(x$State), rankall_helper)
  # states <- unique(x$State)
  # lapply(split(x, x$State), rankall_helper)
  # do.call(rbind, lapply(split(x, x$State), rankall_helper))'
  #by(x, x$State, rankall_helper)
  do.call(rbind, by(x, x$State, rankall_helper))
  ## do.call(rbind, by(x, x$State, function(x)
  ##         x[order(x[,colname], x$Hospital.Name),c('Hospital.Name', 'State')][num,]))
}

