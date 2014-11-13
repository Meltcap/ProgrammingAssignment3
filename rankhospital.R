rankhospital <- function(state, outcome, num = "best") {
    colNameToNumber <- c(11, 17, 23)
    names(colNameToNumber) <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that outcome is valid
    colOutcome <- colNameToNumber[outcome]
    if (is.na(colOutcome)) stop("invalid outcome")
    
    ## Check that num is valid    
    if (is.na(num) 
        || is.numeric(num) && num <= 0 
        || is.character(num) && num != "best" && num != "worst") 
        stop("invalid num")
    
    ## Read outcome data
    ## 2 = hospital name, 7 = state
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeData <- data[data[, 7] == state, c(2,colOutcome)]
    
    ## Check if state is valid
    if (nrow(outcomeData) == 0) stop("invalid state")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    suppressWarnings(outcomeData[,2] <- as.numeric(outcomeData[,2]))
    
    ranking <- outcomeData[order(outcomeData[,2],outcomeData[,1], na.last = NA), ][,1]
    rank <- 1
    if (num == "best") rank <- 1 
    else if (num == "worst") rank <- length(ranking)
    else rank <- num
    
    ranking[rank]
}