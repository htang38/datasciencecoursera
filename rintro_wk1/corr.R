# corr.R
corr <- function(directory, threshold = 0) {
  corr_vec <- c() # correlation vector
  
  for (i in 1:332) { # loop through all csv files
    csv_name <- sprintf("%03d.csv", i) # 0-pad to 3 digits
    dir <- paste(directory, "/", csv_name, sep = "") # directory path string
    df <- read.csv(dir) # read data into dataframe
    good_df <- na.omit(df) # get rid of rows with any NAs
    
    if (nrow(good_df) > threshold) {
      sulf <- good_df[["sulfate"]] # sulfate vector
      nitr <- good_df[["nitrate"]] # nitrate vector
      corr_vec <- append(corr_vec, cor(sulf, nitr))
    }
  }
  
  if (length(corr_vec) == 0) {
    vector(mode = "numeric", length = 0)
  } else {
    corr_vec
  }
}