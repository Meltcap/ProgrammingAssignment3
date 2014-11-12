best <- function(state, outcome) {
    colNameToNumber <- c(11, 17, 23)
    names(colNameToNumber) <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that outcome is valid
    colOutcome <- colNameToNumber[outcome]
    if (is.na(colOutcome)) stop("invalid outcome")
    
    ## Read outcome data
    ## 2 = hospital name, 7 = state
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeData <- data[data[, 7] == state, c(2,colOutcome)]
    
    ## Check if state is valid
    if (nrow(outcomeData) == 0) stop("invalid state")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    suppressWarnings(outcomeData[,2] <- as.numeric(outcomeData[,2]))
    lowest <- outcomeData[order(outcomeData[,2],outcomeData[,1]), ][1, ]
    
    if (is.na(lowest[,2])) stop("No data available in state '", state, "' on death rate of '", outcome, "'")
    
    lowest[,1]
}
