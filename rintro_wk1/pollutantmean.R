# pollutantmean.R

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  for (i in id) { # loop through all given id's
    csv_name <- sprintf("%03d.csv", id) # 0-pad to 3 digits
    dir <- paste(directory, "/", csv_name, sep = "") # directory path string
  }
  
  df = data.frame() # create empty dataframe
  
  for (i in dir) {
    # read csv file into dataframe and add to existing
    df <- rbind(df, read.csv(i))
  }
  
  df <- df[pollutant] # only show given pollutant
  good_df <- na.omit(df) # remove NAs
  mean <- mean(good_df[[pollutant]]) # calculate mean
  
  print(mean)
}
