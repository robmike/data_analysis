count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
## Check that specific "cause" is allowed; else throw error
## Read "homicides.txt" data file
## Extract causes of death
## Return integer containing count of homicides for that cause
  causelist <- c("asphyxiation", "blunt force", "other",  "shooting", "stabbing", "unknown")
  stopifnot(!is.null(cause), cause %in% causelist)

  homicides <- readLines("homicides.txt")
  rx <- paste('cause:', cause)
  length(grep(rx, homicides, ignore.case = TRUE))
}


