rankhospital <- function(state, outcomes, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes[, 11] <- as.numeric(outcomes[, 11])
  outcomes[, 17] <- as.numeric(outcomes[, 17])
  outcomes[, 23] <- as.numeric(outcomes[, 23])
  
  ## Check that state and outcome are valid
  ## Check that state and outcome are valid
  if (!state %in% names(table(outcomes$State))) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
    stop("invalid outcome")
  }  
  
  ## Return hospital name in that state with the given rank
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  lookup <- c()
  lookup["heart attack"] = 11
  lookup["heart failure"] = 17
  lookup["pneumonia"] = 23
  outcomes <- outcomes[outcomes[7] == state,]
  #outcomes <- outcomes[complete.cases(outcomes[,outcomes[[lookup[outcome]]]]),]
  outcomes <- outcomes[complete.cases(outcomes),]
  sortedhospitals <- outcomes[order(outcomes[[lookup[outcome]]],outcomes[2]),]    
  
  if (num=="best") {
    finalnum<-1
  } else if (num=="worst") {
    finalnum<-nrow(sortedhospitals)
  } else {
    finalnum <- num
  }
  #sortedhospitals
  sortedhospitals[2][,1][finalnum]  
  ## 30-day death rate
}
