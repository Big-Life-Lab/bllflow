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
#' @return Returns a new BllFlow object with the data, variables and
#' variableDetailsSheet args attached as variables.
#' @export
#'
#' @examples
BLLFlow <- function(data, variables, variableDetailsSheet) {
  # Verify passed arg integrity for future functions
  CheckIfDataFrame(data,"data")
  CheckIfDataFrame(variables,"variables")
  CheckIfDataFrame(variableDetailsSheet,"variableDetailsSheet")
  CheckForColumnPresence(c("min","max","outlier"),variables,"variables")
  
  retClass <- list(data = data, variables = variables, variableDetailsSheet = variableDetailsSheet)
  attr(retClass, "class") <- "BLLFlow"
  return(retClass)
}
