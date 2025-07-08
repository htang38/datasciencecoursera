# rankall.R

rankall <- function(outcome, num = "best") {
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
  
  # clean up table and format to order by outcome_col
  best_hosp_df <- as.data.frame(best_hosp_data) # transform table into dataframe
  best_hosp_df$outcome_col <- as.numeric(best_hosp_df$outcome_col)
  
  # sort table by state, then by death rates, then by hospital name
  best_hosp_df <- best_hosp_df[with(best_hosp_df, 
                                    order(state_col, outcome_col, hosp_name)),]
  best_hosp_df <- na.omit(best_hosp_df) # get rid of NAs
  
  # create empty data frame for resulting ranks
  allrank_df <- data.frame(matrix(nrow = 54, ncol = 2))
  colnames(allrank_df) <- c("hospital", "state")

  for (i in 1:54) { # repeat for each state/territory
    # find current state
    curr_state <- best_hosp_df[1,2]
    
    # find how many hospitals in each state by finding the number of hospitals
    # with states that match the state of the 1st row of the dataframe
    num_hospitals <- sum(best_hosp_df$state_col == curr_state)
    
    # translate num = "best" and num = "worst" to numbers based on that state
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
        rank <- num_hospitals
    } else { 
      if (num > 0 && num < num_hospitals) { # valid ranking for that state
        rank <- num
      } else { # invalid ranking for that state
        rank <- NA
      }
    }
    
    if (is.numeric(rank)) { # if a hospital in that state has that rank
      allrank_df[i, 1] <- best_hosp_df[rank, 1] # transfer hospital name over
    } else { # hospital name is NA
      allrank_df[i, 1] <- NA
    }
    
    allrank_df[i, 2] <- curr_state # transfer state over

    # get rid of all hospitals with the current state
    best_hosp_df <- best_hosp_df[best_hosp_df[,2] != curr_state,]
  }
  
  print(allrank_df)
}