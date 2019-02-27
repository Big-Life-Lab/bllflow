#' Creates a BLLFlow instance from the data arg
#'
#' @param data A dataframe that represents the dataset the model will be developed
#' on
#' @param variables The specification sheet for this model. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=0.
#' CSV file should be read in using the read.csv function.
#' @param variableDetailsSheet The variable details worksheet. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036.
#' CSV file should be read in using the read.csv function. 
#' The paramater that is passed is the output of the read.csv function
#' @return Returns the data argument with BLLFlow attached as a class name.
#' Adds the variables and variableDetailsSheet args as variables on the
#' data argument.
#' @export
#'
#' @examples
BLLFlow <- function(data, variables, variableDetailsSheet) {
  retClass <- list(data = data, variables = variables, variableDetailsSheet = variableDetailsSheet)
  attr(retClass, "class") <- "BLLFlow"
  return(retClass)
}