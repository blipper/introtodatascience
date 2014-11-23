agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {
    stop("Can't be NULL")
  }  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")  
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  exp = paste(".* (",age,") years old",sep="")
  #print(exp)
  
  ## Return integer containing count of homicides for that age
  myMatches = regexpr(exp, homicides, ignore.case=TRUE)
  ## Return integer containing count of homicides for that cause
  q = attr(myMatches,"match.length")
  length(q[q>-1])
  
  
}