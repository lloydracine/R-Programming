## Programming Assignment 1: Air Pollution
##
## calculates the mean of a pollutant (sulfate or nitrate) across a specified 
## list of monitors
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  m <- c()
  
  #loop through entries in "id" vector
  for (i in id) {
    #setup file name
    path <-
      file.path(directory, paste(sprintf("%03d",as.numeric(i)),".csv",sep = ""))
    
    #read data
    data <- read.csv(path)
    
    #identify missing "pullutant" data
    b <- is.na(data[[pollutant]])
    
    #put "pulluntant" data in vector
    m <- c(m,data[[pollutant]][!b])
  }
  
  #return mean of "pullutant" rounded to 3 decimal places
  return (mean(m))
}

# example tests
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)