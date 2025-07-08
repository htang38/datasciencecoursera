# rankhospital.R
library(tidyverse)

rankhospital <- function(state, outcome, num = "best") {
  # read .csv file
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # get hospital name and state column from table
  hosp_name <- data[,2]
  state_col <- data[,7]
  
  # get outcome column from table
  if (outcome == "heart attack") {
    outcome_col <- data[,11]
  } else if (outcome == "heart failure") {
    outcome_col <- data[,17]
  } else if (outcome == "pneumonia") {
    outcome_col <- data[,23]
  } else { # else, invalid outcome; print error
    stop("invalid outcome")
  }
  
  # combine columns
  best_hosp_data <- cbind(hosp_name, state_col, outcome_col)
  
  # get rows only with given state
  if (!(state %in% best_hosp_data[,2])) {
    stop("invalid state")
  } else {
    best_hosp_data <- best_hosp_data[best_hosp_data[,2] == state,]
  }
  
  # clean up table and format to order by outcome_col
  best_hosp_df <- as.data.frame(best_hosp_data) # transform table into dataframe
  best_hosp_df$outcome_col <- as.numeric(best_hosp_df$outcome_col)
  
  # sort table by death rates first, then by hospital name (to order ties)
  best_hosp_df <- best_hosp_df[with(best_hosp_df, 
                                    order(outcome_col, hosp_name)),]
  best_hosp_df <- na.omit(best_hosp_df) # omit rows with NAs
  
  # find hospital with the given rank
  if (num == "best") {
    rank_hosp <- best_hosp_df[1, 1] # return best hospital
  } else if (num == "worst") {
    rank_hosp <- best_hosp_df[nrow(best_hosp_df), 1] # return worst hospital
  } else if (is.numeric(num)) { # given rank is a number
    
    # check if number is within bounds
    if (num > 0 && num <= nrow(best_hosp_df)) { # valid index
      rank_hosp <- best_hosp_df[num, 1]
    }
  } else { # invalid non-numeric rank
    rank_hosp <- NA
  }
  
  print(rank_hosp)
  
}