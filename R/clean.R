#' Cleans the data using the min and outlier columns in the variables sheet of
#' the MSW. Outlier method is applied on a row if any of the variable
#' values for that row is less than the min value as specified in the variables
#' sheet. Outlier checking for the column is not applied if min value is NA.
#' 
#' Currently supported outlier methods are:
#' Delete - Specified as 'delete' in MSW. Deletes the row from the data. 
#' Deleted rows are stored in the metadata variable under the deletedRows name.
#' Missing - Specified as 'missing' in MSW. Column value for that row which does 
#' not meet the criteria is set to NA.
#' Not Applicable - TODO.
#' Set to value - Specified as a number value in MSW. Column value for the row is 
#' set to the value specified in the outlier column.
#'
#' @param bllFlowModel bllFlow model created using the BLLFlow method
#' @param print=FALSE If set true, prints the following metrics about the model:
#' 1. Number of rows deleted from the dataset 
#' 
#' @return
#' @export
#'
#' @examples
clean.min.BLLFlow <- function(bllFlowModel, print = FALSE) {
  
}

#' Cleans the data using the max and outlier columns in the variables sheet of
#' the MSW. Outlier method is applied on a row if any of the variable
#' values for that row is greater than the max value as specified in the variables
#' sheet. Outlier checking for the column is not applied if max value is NA.
#' 
#' Currently supported outlier methods are:
#' Delete - Specified as 'delete' in MSW. Deletes the row from the data. 
#' Deleted rows are stored in the metadata variable under the deletedRows name.
#' Missing - Specified as 'missing' in MSW. Column value for that row which does 
#' not meet the criteria is set to NA.
#' Not Applicable - TODO.
#' Set to value - Specified as a number value in MSW. Column value for the row is 
#' set to the value specified in the outlier column.
#'
#' @param bllFlowModel bllFlow model created using the BLLFlow method 
#' @param print=FALSE If set to true, prints the following metrics about the model:
#' 1. Number of rows deleted from the dataset 
#' 
#' @return
#' @export
#'
#' @examples
clean.max.BLLFlow <- function(bllFlowModel, print = FALSE) {
  
}