#' Makes the data argument an instance of 'bll_flow'
#'
#' @param data A dataframe that represents the dataset the model will be developed
#' on
#' @param MSW The master specification sheet for this model. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1516824401.
#' CSV file should be read in using the read.csv function.
#' @param variable_details_sheet The variable details worksheet. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036.
#' CSV file should be read in using the read.csv function.
#' @return Returns the data argument now an instance of the bll_flow class with
#' the MSW and variable_details_sheet as variables on the object.
#' @export
#'
#' @examples
bll_flow <- function(data, MSW, variable_details_sheet) {
  
}