count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Can't be NULL")
  }
  ## Check that specific "cause" is allowed; else throw error
  causes = c("asphyxiation", "blunt force", "other", "shooting","stabbing", "unknown")
  if (!cause %in% causes) {
    stop("Not known cause")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")  
  
  exp = paste(".*Cause: (",cause,")<.*",sep="")
  ## Extract causes of death
  myMatches = regexpr(exp, homicides, ignore.case=TRUE)
  ## Return integer containing count of homicides for that cause
  q = attr(myMatches,"match.length")
  length(q[q>-1])
}
