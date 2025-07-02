# complete.R
complete <- function(directory, id = 1:332) {
  for (i in id) { # loop through all given id's
    csv_name <- sprintf("%03d.csv", id) # 0-pad to 3 digits
    dir <- paste(directory, "/", csv_name, sep = "") # directory path string
  }
  
  # make data frame to store data with columns and correct # of rows
  table <- data.frame(id = id)
  nobs <- c() # empty vector for nobs column
  
  for (i in dir) { # loop through all csv files
    df <- read.csv(i) # read data into dataframe
    good_df <- na.omit(df) # get rid of rows with any NAs
    
    # number of rows = # of completely observed cases
    # append # of completely observed cases to end of vector
    nobs <- append(nobs, nrow(good_df)) 
  }
  
  # add nobs vector to table
  table <- cbind(table, nobs)
  
  print(table)
}