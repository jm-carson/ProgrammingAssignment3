# Programming Assignment 3: Hospital Quality
# Part 4: Ranking hospitals in all states

# This function has two arguments: an outcome name and a hospital ranking (best, worst, or integer)

# Reads the 'outcome-of-care-measures.csv' file and returns the name of the hospital that has the
# specified ranking.

# Outcomes can be 'heart attack', 'heart failure', or 'pneumonia'.
# Hospital name is the name provided in the Hospital.name variable.
# Hospitals that do not have data on a particular outcome are excluded.
# If there is a tie, first hospital in alphabetical order is chosen.

# setwd("//shshome/Personal/JAM59536/My Documents/R/Coursera Data Science Specialization/Data")

rankall <- function(outcome, num = "best") {
  
          # Read the outcome data
          data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
          
          # Check that outcome is valid
          if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
            stop("Invalid outcome")
          }
          
          # Match column name to appropriate outcome  
          actual_col_name <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")  
          
          valid_outcome = c("heart attack","heart failure","pneumonia")
          select_col_name <- actual_col_name[match(outcome,valid_outcome)]
          
          # For each state, find the hospital of a the given rank
          state <- levels(factor(data[, 7]))
          hospital <- vector(mode = "character")
          
          for (i in seq(state)) {
                hospital[i] <- rankhospital(state[i], outcome, num)
          }
          
          
          # Return a data frame with the hospital names and state name abbreviations
          data.frame(hospital, state)
}


head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
