rankall <- function(outcome, num = "best") {
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
    
    ## outcomeData[Hospital.Name, State, outcome]
    outcomeData <- data[, c(2, 7, colOutcome)]
    colnames(outcomeData) <- c("Hospital.Name", "State", outcome)
    
    ## convert the death rates from character to numeric
    suppressWarnings(outcomeData[, outcome] <- as.numeric(outcomeData[, outcome]))
    
    rankedData <- outcomeData[order(outcomeData[,"State"], 
                                    outcomeData[, outcome], 
                                    outcomeData[,"Hospital.Name"], na.last = NA), ]
    
    rank <-  function(x) 1
    if (num == "best") rank <- function(x) 1 
    else if (num == "worst") rank <- function(x) nrow(x)
    else rank <- function(x) num
    
    rankedData <- split(rankedData, rankedData[, "State"])
    
    list <- sapply(rankedData, function(x) x[rank(x),"Hospital.Name"])
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    stateRanks <- data.frame(hospital = list, state = names(list), row.names = names(list))
    stateRanks
}

