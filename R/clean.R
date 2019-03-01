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
  bllFlowModel <- ProcessMinOrMax(bllFlowModel, "min",print)
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
  bllFlowModel <- ProcessMinOrMax(bllFlowModel, "max",print)
  return(bllFlowModel)
}

# Internal helper functions ------------------------------------------------------------------

# Create a list of only the variables that need min applied to them
CreateOperationVariableList <-
  function(variable, operation, outlier) {
    if (!is.na(operation)) {
      return(c(variable, operation, outlier))
    }
  }

# Function for actually manipulating the data
ProcessMinOrMax <- function(bllFlowModel, operation, print) {
  # This is to only store rows which contain instructions for Min
  variablesToCheck <-
    apply(bllFlowModel$variables, 1, function(y)
      CreateOperationVariableList(y["variable"], y[operation], y["outlier"]))
  variablesToCheck[sapply(variablesToCheck, is.null)] <- NULL

  # Clean the affected rows
  for (i in variablesToCheck) {
    preRows <- nrow(bllFlowModel$data)
    affectedRows <- 0

    # Handling for the delete outlier
    if (i["outlier"] == "delete") {
      if (as.character(operation) == "min") {
        # remove all the rows that are less then min
        bllFlowModel$data <-
          bllFlowModel$data[!(bllFlowModel$data[i["variable"]] < i[operation] &
            !is.na(bllFlowModel$data[i["variable"]])), ]
      } else if (as.character(operation) == "max") {
        # remove all the rows that are greater then max
        bllFlowModel$data <-
          bllFlowModel$data[!(bllFlowModel$data[i["variable"]] > i[operation] &
            !is.na(bllFlowModel$data[i["variable"]])), ]
      }

      affectedRows <- preRows - nrow(bllFlowModel$data)

      # Handle missing outlier
    } else if (i["outlier"] == "missing") {
      preContainRows <-
        length(which(is.na(bllFlowModel$data[i["variable"]])))

      if (as.character(operation) == "min") {
        bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]] < i[operation]] <-
          NA
      } else if (as.character(operation) == "max") {
        bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]] > i[operation]] <-
          NA
      }

      postRows <-
        length(which(is.na(bllFlowModel$data[i["variable"]])))
      affectedRows <- postRows - preContainRows

      # Handle the replace with outlier
    } else if (!is.na(as.numeric(i["outlier"]))) {
      preContainRows <-
        length(which(bllFlowModel$data[i["variable"]] == i["outlier"]))
      if (as.character(operation) == "min") {
        bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]] < i[operation]] <-
          i["outlier"]
      } else if (as.character(operation) == "max") {
        bllFlowModel$data[i["variable"]][bllFlowModel$data[i["variable"]] > i[operation]] <-
          i["outlier"]
      }
      
      postRows <-
        length(which(bllFlowModel$data[i["variable"]] == i["outlier"]))
      affectedRows <- postRows - preContainRows

      # Handle non supported outlier
    } else {
      print("not supported")
    }

    # Log the the activity of this outlier
    bllFlowModel <-
      LogFunctionActivity(
        bllFlowModel,
        preRows,
        affectedRows,
        i["outlier"],
        paste(i["variable"], " ", operation, " at ", i[operation], sep = ""),
        paste("clean.", operation, ".BLLFlow", sep = ""),
        i[["variable"]],
        i[[operation]],
        print
      )
  }
  return(bllFlowModel)
}
