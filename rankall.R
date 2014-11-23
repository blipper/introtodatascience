myfunc <- function(state, outcome, outcomes, num="best") {  
  lookup <- c()
  lookup["heart attack"] = 11
  lookup["heart failure"] = 17
  lookup["pneumonia"] = 23  
  myoutcomes <- outcomes[outcomes[7] == state,]
  myoutcomes <- myoutcomes[complete.cases(myoutcomes[[lookup[outcome]]]),]
  sortedhospitals <- myoutcomes[order(myoutcomes[[lookup[outcome]]],myoutcomes[2]),]    
  
  if (num=="best") {
    finalnum<-1
  } else if (num=="worst") {
    finalnum<-nrow(sortedhospitals)
  } else {
    finalnum <- num
  }
  #sortedhospitals
  c(sortedhospitals[2][,1][finalnum],state)  
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes[, 11] <- as.numeric(outcomes[, 11])
  outcomes[, 17] <- as.numeric(outcomes[, 17])
  outcomes[, 23] <- as.numeric(outcomes[, 23])
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
    stop("invalid outcome")
  }    
  
  x<-as.data.frame(t(sapply(as.list(names(table(outcomes$State))), myfunc, outcome, outcomes, num)))
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(x) <- c("hospital","state")
  x
}
