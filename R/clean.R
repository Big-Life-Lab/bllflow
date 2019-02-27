#' Cleans the data using the min and outlier columns in the variables sheet of
#' the MSW. Outlier method is applied on a row if any of the variable
#' values for that row is less than the min value as specified in the variables
#' sheet. Outlier checking for the column is not applied if min value is NA.
#'
#' Currently supported outlier methods are:
#' Delete - Specified as 'delete' in MSW. Deletes the row from the data.
#' number of deleted rows as well as their reason for deletion is stored in the 
#' metaData variable under the deletedRows name.
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
CleanMinBLLFlow <- function(bllFlowModel, print = FALSE) {

  # Create a list of only the variables that need min applied to them
  CreateMinVariableList <- function(variable, min, outlier) {
    if (!is.na(min)) {
      return(c(variable, min, outlier))
    }
  }
  # This is to only store rows which contain instructions for Min
  variablesToCheck <- apply(bllFlowModel$variables, 1, function(y) CreateMinVariableList(y["variable"], y["min"], y["outlier"]))
  variablesToCheck[sapply(variablesToCheck, is.null)] <- NULL

  # Clean the affected rows
  for (i in variablesToCheck) {
    # Handling for the delete outlier
    if (i["outlier"] == "delete") {
      preRemoveRows <- nrow(bllFlowModel$data)
      # remove all the rows that are less then min
      bllFlowModel$data <- bllFlowModel$data[!(bllFlowModel$data[i["variable"]] < i["min"] & !is.na(bllFlowModel$data[i["variable"]])), ]
      removedRows <- preRemoveRows - nrow(bllFlowModel$data)
      cat(removedRows, "rows were removed from the data frame following the min function on the", i["variable"], "column due to them being <", i[["min"]], "\n")
      # Create a new deleted Rows list or append to the existing one 
      if (is.null(bllFlowModel$metaData[["deletedRows"]])) {
        bllFlowModel$metaData[["deletedRows"]] <- list(numberOfDeletedRows = removedRows, reasonForDeletion = paste("min at", i[["min"]]))
      } else {
        bllFlowModel$metaData$deletedRows$numberOfDeletedRows <- append(bllFlowModel$metaData$deletedRows$numberOfDeletedRows,  removedRows)
        bllFlowModel$metaData$deletedRows$reasonForDeletion <- append(bllFlowModel$metaData$deletedRows$reasonForDeletion,  paste("min at", i[["min"]]))
        }
    }else if(i["outlier"]=="missing"){
      bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]]<i["min"]]<-NA
    } else if(is.na(as.numeric(i["outliner"]))){
      bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]]<i["min"]]<-i["outlier"]
    } else{
      print('not supported')
    }
  }
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
