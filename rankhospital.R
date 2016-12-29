# Programming Assignment 3: Hospital Quality
# Part 3: Ranking hospitals by outcome in a state

# This function has three arguments: the 2-character abbreviated name of a state, an outcome name,
# and the ranking (best, worst, or integer) of a hospital in that state for that particular outcome.

# Reads the 'outcome-of-care-measures.csv' file and returns the name of the hospital that has the
# specified ranking.

# Outcomes can be 'heart attack', 'heart failure', or 'pneumonia'.
# Hospital name is the name provided in the Hospital.name variable.
# Hospitals that do not have data on a particular outcome are excluded.
# If there is a tie, first hospital in alphabetical order is chosen.

# setwd("//shshome/Personal/JAM59536/My Documents/R/Coursera Data Science Specialization/Data")

rankhospital <- function(state, outcome, num = "best") {
        
                # Read the outcome data
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
                
                # Check that state is valid
                if (!state %in% data[, "State"]) {
                  stop("Invalid state")
                }
                
                # Check that outcome is valid
                if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                  stop("Invalid outcome")
                }
          
                # Subset data for selected state
                data_selected_state <- subset(data, State == state)
                
                # Match column name to appropriate outcome  
                actual_col_name <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                                     "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")  
                
                valid_outcome = c("heart attack","heart failure","pneumonia")
                select_col_name <- actual_col_name[match(outcome,valid_outcome)]
                
                # Sort data by outcome
                sorted_data <- suppressWarnings(data_selected_state[order(as.numeric(data_selected_state[[select_col_name]]),data_selected_state[["Hospital.Name"]], decreasing=FALSE, na.last=NA), ])
                
                # Return hospital name in chosen state with specified rank for chosen outcome
                if (num == "best") num = 1
                if (num == "worst") num = nrow(sorted_data)
                sorted_data[num, "Hospital.Name"]
  
}


rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "best")
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
