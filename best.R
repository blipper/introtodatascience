best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes[, 11] <- as.numeric(outcomes[, 11])
  outcomes[, 17] <- as.numeric(outcomes[, 17])
  outcomes[, 23] <- as.numeric(outcomes[, 23])
  
  ## Check that state and outcome are valid
  if (!state %in% names(table(outcomes$State))) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  lookup <- c()
  lookup["heart attack"] = 11
  lookup["heart failure"] = 17
  lookup["pneumonia"] = 23
  outcomes <- outcomes[outcomes[7] == state,]
  
  sortedhospitals <- outcomes[order(outcomes[[lookup[outcome]]],outcomes[2]),]    
  sortedhospitals[2][,1][1]
}
