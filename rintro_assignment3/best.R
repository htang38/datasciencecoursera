# best.R

best <- function(state, outcome) {
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
  if (!state %in% best_hosp_data[,2]) { # if state invalid
    stop("invalid state")
  }
  
  # find lowest 30-day death rate
  outcome_vec <- as.numeric(best_hosp_data[,3])
  outcome_vec <- na.omit(outcome_vec)
  min_death_rate = min(outcome_vec)
  
  # find hospital with this death rate
  best_hosp <- (best_hosp_data[best_hosp_data[,3] == min_death_rate])[1]
  
  # print
  print(best_hosp)
}
