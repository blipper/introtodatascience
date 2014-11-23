getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  
  ## Your code here
  myData <- read.csv(paste(directory,"/",formatC(id, width=3,digits=3,flag="0"),".csv",sep=""))
  if (summarize==TRUE) {
    print(summary(myData))
  }
  myData    
}


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

repFun <- function(sulnit) {
  cor(sulnit[[1]],sulnit[[2]],use="pairwise.complete.obs")
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
    
  ## Return a numeric vector of correlations
  myObs <- complete(directory)
  myFilteredObs <- myObs[myObs$nobs>threshold,"id"]
  filtered <- as.data.frame(t((sapply(1:332, getmonitor,directory))))
  #Qunique(unlist(filtered$ID)) %in% myFilteredObs
  #filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"sulfate"]
  #filtered[filtered$ID %in% myFilteredObs,"sulfate"]
  #cbind(filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"sulfate"], filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"nitrate"])
  #mapply(c,filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"sulfate"], filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"nitrate"])
  apply(filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,c("sulfate","nitrate")],1,repFun)
  #sulfates <- c(filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"sulfate"], recursive = TRUE)
  #nitrates <- c(filtered[unique(unlist(filtered$ID)) %in% myFilteredObs,"nitrate"], recursive = TRUE)
  #cor(sulfates,nitrates,use="complete.obs")
}


