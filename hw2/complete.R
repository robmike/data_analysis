complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
  completehelper <- function(directory) {
    function(id) {
      data <- getmonitor(id, directory)
      nrow(na.omit(data))
    }
  }

  f <- completehelper(directory)
  nobs <- sapply(id, f)
  out <- data.frame(cbind(id=id, nobs=nobs))
  out
}
