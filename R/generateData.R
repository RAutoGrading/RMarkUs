library(utils)

# Purpose: Randomized data generation

#' Data Extraction
#'
#' Extract the data from the file
#' @param file Filename of the full data set
#' @param sep The field separator character. Values on each line of the \
#' file are separated by this character. If sep="", the separator is 'white space',
#' that is one or more spaces, tabs, newlines, or carriage returns.
#' Permitted values are: ",", "", and tab (backslash t); defaults to ",".
#' @param ... Further arguments to be passed to the function
#' @return The data extracted from the file as a data.frame
#' @export
extractData <- function(file, sep=",", ...){
  if(sep==","){
    data <- read.csv(file=file, sep=sep, ...)
  } else if(sep==" "){
    data <- read.table(file=file, sep=sep, ...)
  } else if(sep=="\t"){
    data <- read.delim(file = file, sep = sep, ...)
  }
  else{
    stop("Invalid value of `sep` passed to extractData function.")
  }
  return(data)
}# end extractData


#' Random Subset
#'
#' Randomly subset data from a full dataset
#' @param file Filename of full dataset
#' @param sep The field separator character. Values on each line of the \
#' file are separated by this character. If sep="", the separator is 'white space',
#' that is one or more spaces, tabs, newlines, or carriage returns.
#' Permitted values are: ",", "", and tab (backslash t); defaults to ",".
#' @param size Number of rows to choose for the subset.
#' @param variables Variables to select from data set.
#' DEFAULT is all.
#' @param seedNum Seed to randomly select data from data set
#' @param ... Further arguments to be passed to the function
#' @return The random subset of data from the full dataset
#' @export

subsetData <- function(file, sep, size, variables="all", seedNum, ...){
  data <- extractData(file=file, sep=sep, ...)

  if(size >= nrow(data)){
    stop(paste("To get a random subset from a dataset, size should be smaller than the number of rows, but here you passed size=", size, " and the full data has ", nrow(data), " rows."))
  }

  set.seed(seedNum)
  random_rows <- sample(1:nrow(data), size)

  if(variables != "all"){
    data <- data[variables]
  }

  randomData <- data[random_rows,]

  return(randomData)
} # end subsetData function



#' Probability Distribution
#'
#' Helper function to run the distribution function using the string name
#'
#' @param fct Probability distribution function in R as a string
#' @param ... Parameters for function
#' @return List of random values generated from probability distribution
#' @examples
#' vals1 <- run_function('rnorm', 10, 0, 1)
#' vals2 <- run_function('rbinom', 10, 2, 0.5)
#' @export
# run_function

run_function <- function(fct, ...) {
  get(fct)(...)
}

#' Generate Data
#'
#' Generate data from a random probability distribution
#' @param distribution Distribution to generate data from.
#' Options are: 'beta', 'binom', 'chisq', 'exp', 'f', 'gamma', 'geom', 'hyper',
#' 'logis', 'lnorm', 'nbinom', 'norm', 'pois', 't', 'unif', 'weibull'
#' Example: [0,1] for mean and standard deviation of Normal distribution
#' @param n Size of dataset
#' @param seedNum Seed to randomly select data from data set
#' @param ... The remaining parameters to set based on random distribution
#' @return Data frame with randomly generated data from specified probability distribution
#' @examples
#' vals1 <- generateData('norm', 10, 25, mean=0, sd=1)
#' vals2 <- generateData('binom', 10, 25, size=2, prob=0.5)
#' @export
# Generate data

generateData <- function(distribution, n, seedNum, ...){
  set.seed(seedNum)
  distr_fnc <-  paste('r', distribution, sep='')
  randomData <- run_function(distr_fnc, n, ...)

  return(as.data.frame(randomData))
}
