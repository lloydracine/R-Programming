corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  # get the complete table
  complete_table <- complete(directory, 1:332)
  nobs <- complete_table$nobs
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  
  cv <- c()
  
  #loop through entries in "id" vector
  for (i in ids) {
    #setup file name
    path <-
      file.path(directory, paste(sprintf("%03d",as.numeric(i)),".csv",sep = ""))
    
    #read data
    data <- read.csv(path)
    
    #put complete.case into data vector
    cv <- c(cv,cor(data$sulfate, data$nitrate, use="complete.obs"))
  }    

  return(cv)  
}

  
# example tests
#source("corr.R")
#source("complete.R")
#cr <- corr("specdata", 150)
#head(cr)
#summary(cr)
#cr <- corr("specdata", 400)
#head(cr)
#summary(cr)
#cr <- corr("specdata", 5000)
#summary(cr)
#length(cr)
#cr <- corr("specdata")
#summary(cr)
#length(cr)
