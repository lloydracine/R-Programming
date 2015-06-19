## Programming Assignment 3
##
## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking specified 
## by the num argument.
## 
## If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data 
## on a particular outcome should be excluded from the set of hospitals when 
## deciding the rankings.
##
## It may occur that multiple hospitals have the same 30-day mortality rate for 
## a given cause of death. In those cases ties should be broken by using the 
## hospital name.

rankhospital <- function(state, outcome, num = "best") {
  ## 'state' 2-character abbreviated name of a state.  If an invalid state value 
  ## is passed to best, the function should throw an error via the stop function 
  ## with the exact message "invalid state".
  
  ## 'outcome' Outcome name.  Can be one of "heart attack", "heart failure", 
  ## or "pneumonia".  If an invalid outcome value is passed to the function 
  ## should throw an error via the stop function with the exact message 
  ## "invalid outcome".
  
  ## 'num' the ranking of a hospital in the state for the outcome. Can take 
  ## values "best", "worst", or an integer indicating the ranking (smaller 
  ## numbers are better).

  ## returns a character vector with the name of the hospital that has the best 
  ## (i.e. lowest) 30-day mortality for the specified outcome in that state
  
  ## Reads outcome data
    file_data <- read.csv("outcome-of-care-measures.csv", sep = ",")
    
    ## Checks that state and outcome are valid
    valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(state, valid_states)) stop("invalid state")
    if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
    
    ## Returns hospital name in that state with lowest 30-day death
    data <- file_data[file_data$State == state,]
    header_name <- NULL
    if (outcome == "heart attack") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"

    sorted_data <- data[order(as.numeric(as.character(data[,header_name])), as.character(data[,"Hospital.Name"])),]
    sorted_data <- sorted_data[!sorted_data[,header_name] == "Not Available",]
    if (num == "best") {
        return(best(state, outcome))
    } else if (num == "worst") {
        return(tail(as.character(sorted_data[,"Hospital.Name"]), n = 1))
    }
    return(as.character(sorted_data[,"Hospital.Name"][num]))
}