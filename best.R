# Programming Assignment 3: Hospital Quality
# Part 2: Finding the best hospital in a state

# This function has two arguments: the 2-character abbreviated name of a state and an outcome name.
# Reads the 'outcome-of-care-measures.csv' file and returns the hospital with the lowest 30-day 
# mortality for the specified outcome.

# Outcomes can be 'heart attack', 'heart failure', or 'pneumonia'.
# Hospital name is the name provided in the Hospital.name variable.
# Hospitals that do not have data on a particular outcome are excluded.
# If there is a tie, first hospital in alphabetical order is chosen.

# setwd("//shshome/Personal/JAM59536/My Documents/R/Coursera Data Science Specialization/Data")

best <- function(state, outcome) {
        
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
  
        # Return hospital name in that state with lowest 30-day mortality
        min_val <- suppressWarnings(which.min(as.double(data_selected_state[,select_col_name])))
        output <- data_selected_state[min_val,"Hospital.Name"]
        return(output)

        
}


best("TX", "heart attack")
