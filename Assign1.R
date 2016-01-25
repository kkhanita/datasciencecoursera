#Assignment 1
#part1
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
  files_full <- list.files(directory, full.names = TRUE) 
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }

  
  mean(dat[, pollutant], na.rm = TRUE)
}


complete <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  
  for (i in id) {
    moni_i <- read.csv(files_full[i])
    nobs <- sum(complete.cases(moni_i))
    tmp <- data.frame(i, nobs)
    dat <- rbind(dat, tmp)
  }
  
  colnames(dat) <- c("id", "nobs")
  dat
}


corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat
}

