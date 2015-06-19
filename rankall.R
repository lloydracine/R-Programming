## Programming Assignment 3
##
## The function reads the outcome-of-care-measures.csv file and returns a 
## a 2-column data frame containing the hospital in each state that has the 
## ranking specified in num.
## 
## If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data 
## on a particular outcome should be excluded from the set of hospitals when 
## deciding the rankings.
##
## The first column in the data frame is named hospital, which contains the 
## hospital name, and the second column is named state, which contains the 
## 2-character abbreviation for the state name. Hospitals that do not have 
## data on a particular outcome should be excluded from the set of hospitals 
## when deciding the rankings. 

rankall <- function(outcome, num = "best") {
  ## 'outcome' Outcome name.  Can be one of "heart attack", "heart failure", 
  ## or "pneumonia".    If an invalid outcome value is passed to the function 
  ## should throw an error via the stop function with the exact message 
  ## "invalid outcome".
  
  ## 'num' the ranking of a hospital in the state for the outcome. Can take 
  ## values "best", "worst", or an integer indicating the ranking (smaller 
  ## numbers are better).
  
  ## returnS a data frame containing the names of the hospitals that are the 
  ## best in their respective states for 30-day heart attack death rates. 
  ## The function should return a value for every state (some may be NA).
  
  ## Reads outcome data
    file_data <- read.csv("outcome-of-care-measures.csv", sep = ",")
    
    ## Checks that state and outcome are valid
    valid_states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
    
    header_name <- NULL
    if (outcome == "heart attack") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
    hosps <- c()
    states <- c()
    
    ## For each state, finds the hospital of the given rank
    for (state in valid_states) {
        ranked_hosp <- c()
        data <- file_data[file_data$State == state,]        
        sorted_data <- data[order(as.numeric(as.character(data[,header_name])), as.character(data[,"Hospital.Name"])),]
        sorted_data <- sorted_data[!sorted_data[,header_name] == "Not Available",]
        if (num == "best") {
            ranked_hosp <- best(state, outcome)
        } else if (num == "worst") {
            ranked_hosp <- as.character(tail(sorted_data[,"Hospital.Name"], n = 1))
        } else {
            ranked_hosp <- as.character(sorted_data[,"Hospital.Name"][num])
        }
        hosps <- c(hosps, ranked_hosp)
    }
    result <- data.frame(hosps, valid_states)
    colnames(result) <- c("hospital", "state")
    return(result)
}