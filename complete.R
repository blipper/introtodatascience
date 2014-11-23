fn <- function(id, directory) {
  zero <- sprintf("%03d", id)
  name <- paste(directory,"/",zero,".csv",sep="")
  frame <- read.csv(name)
  ok <- complete.cases(frame)
  return(c("nobs"=sum(ok),"id"=id)) }

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
  myframes = sapply(id, fn, directory)
  as.data.frame(t(myframes))
  #sapply(split(myframes$sulfate, myframes$ID), mean)
  #mycounts = lapply(myframes, function (myframe) {c(myframe$,count(complete.cases(myframe)))})
}
