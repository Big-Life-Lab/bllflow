#' Makes the data argument an instance of 'bll_flow'
#'
#' @param data A dataframe that represents the dataset the model will be developed
#' on
#' @param MSW The master specification sheet for this model. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1FGy-Zg0AUMEVSqpbAoIMoS_SgD__mG2SCIU2vVwsyCI/edit?ts=5c49dc3d#gid=1367487357.
#' CSV file should be read in using the read.csv function.
#' @param variable_details_sheet The variable details worksheet. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1FGy-Zg0AUMEVSqpbAoIMoS_SgD__mG2SCIU2vVwsyCI/edit?ts=5c49dc3d#gid=1848193100.
#' CSV file should be read in using the read.csv function.
#' @return Returns the data argument now an instance of the bll_flow class with
#' the MSW and variable_details_sheet as variables on the object.
#' @export
#'
#' @examples
bll_flow <- function(data, MSW, variable_details_sheet) {
  
}