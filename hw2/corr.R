corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
  corrhelper <- function(id) {
    data <- na.omit(getmonitor(id, directory))
    cor(data[,'sulfate'], data[,'nitrate'])
  }

  ncomp <- complete(directory)
  id <- ncomp[ncomp$nobs > threshold, 'id']
  sapply(id, corrhelper)
}
