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
#' @return bllFlowModel that has had its data modified by the paramaters located in
#' the variables object
#' @export
#'
#' @examples
clean.Min.BLLFlow <- function(bllFlowModel, print = FALSE) {
  
  bllFlowModel <-
    ProcessMinOrMax(bllFlowModel, "min", print, CheckLessThen)

  return(bllFlowModel)
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
#' @return bllFlowModel that has had its data modified by the paramaters located in
#' the variables object
#' @export
#'
#' @examples
clean.Max.BLLFlow <- function(bllFlowModel, print = FALSE) {
  
  bllFlowModel <-
    ProcessMinOrMax(bllFlowModel, "max", print, CheckGreaterThen)

  return(bllFlowModel)
}

# Internal helper functions ------------------------------------------------------------------

# Create a list of only the variables that need min applied to them
CreateOperationVariableList <-
  function(variable, operation, outlier) {
    
    if (!is.na(operation)) {
      return(list(variable, operation, outlier))
    }
  }

# Less then comparing function
CheckLessThen <- function(operatorBasedCompareValue, valueBeingCompare) {
  
  return(operatorBasedCompareValue < valueBeingCompare)
}

# Greater then comparing function
CheckGreaterThen <- function(operatorBasedCompareValue, valueBeingCompare) {
  
  return(operatorBasedCompareValue > valueBeingCompare)
}
# Function for actually manipulating the data
ProcessMinOrMax <-
  function(bllFlowModel,
             operation,
             print,
             PerformRowCheck) {
    
    # This is to only store rows which contain instructions for Min
    # This is done to avoid parsing through unafected variables
    variablesToCheck <-
      apply(bllFlowModel$variables, 1, function(y)
        CreateOperationVariableList(y["variable"], y[operation], y["outlier"]))
    variablesToCheck[sapply(variablesToCheck, is.null)] <- NULL

    # Check if all the variables from variables to check exist in the data
    CheckForExistanceOfInList(variablesToCheck, colnames(bllFlowModel$data))

    # Clean the affected rows
    for (variableRowBeingChecked in variablesToCheck) {
      numTotalRows <- nrow(bllFlowModel$data)
      numAffectedRows <- 0

      # Handling for the delete outlier
      if (variableRowBeingChecked[[3]]["outlier"] == "delete") {
        # Remove all rows that pass the rowCheck
        bllFlowModel$data <-
          bllFlowModel$data[!(
            PerformRowCheck(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]], variableRowBeingChecked[[2]][operation]) &
              !is.na(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]])
          ), ]

        numAffectedRows <- numTotalRows - nrow(bllFlowModel$data)

        # Handle missing outlier
      } else if (variableRowBeingChecked[[3]]["outlier"] == "missing") {
        numPreContainRows <-
          length(which(is.na(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]])))
        bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]][PerformRowCheck(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]], variableRowBeingChecked[[2]][operation])] <-
          NA
        numPostRows <-
          length(which(is.na(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]])))
        numAffectedRows <- numPostRows - numPreContainRows

        # Handle the replace with outlier
      } else if (!is.na(as.numeric(variableRowBeingChecked[[3]]["outlier"]))) {
        numPreContainRows <-
          length(which(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]] == variableRowBeingChecked[[3]]["outlier"]))
        bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]][PerformRowCheck(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]], variableRowBeingChecked[[2]][operation])] <-
          variableRowBeingChecked[[3]]["outlier"]
        numPostRows <-
          length(which(bllFlowModel$data[variableRowBeingChecked[[1]]["variable"]] == variableRowBeingChecked[[3]]["outlier"]))
        numAffectedRows <- numPostRows - numPreContainRows

        # Handle non supported outlier
      } else {
        stop(paste("Unsupported outlier method ", variableRowBeingChecked[[3]]["outlier"]))
      }

      # Log the the activity of this outlier
      bllFlowModel <-
        LogFunctionActivity(
          bllFlowModel,
          numTotalRows,
          numAffectedRows,
          variableRowBeingChecked[[3]]["outlier"],
          paste(
            variableRowBeingChecked[[1]]["variable"],
            " ",
            operation,
            " at ",
            variableRowBeingChecked[[2]][operation],
            sep = ""
          ),
          paste("clean.", operation, ".BLLFlow", sep = ""),
          variableRowBeingChecked[["variable"]],
          variableRowBeingChecked[[operation]],
          print
        )
    }

    return(bllFlowModel)
  }
