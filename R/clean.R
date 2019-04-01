#' Clean.Min
#'
#' Clean The data using the Min according to the specifications in the passed object.
#'
#' @param  modelToClean A passed object that contains the data that needs to be cleaned
#' as well as specifications on how to clean it.
#' @param print If set true prints human readable log of the performed cleaning operations
#'
#' @return The passed object with now cleaned data as well as a log containing the performed actions
#' @export
clean.Min <- function(modelToClean, print = FALSE) {
  UseMethod("clean.Min", modelToClean)
}
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
#' @param print If set true, prints the following metrics about the model:
#' 1. Number of rows deleted from the dataset
#'
#' @return bllFlowModel that has had its data modified by the paramaters located in
#' the variables object
#' @export
#'
#' @examples
#' # Install the packages
#'
#' # Read in the data we will use
#'
#' library(survival)
#' data(pbc)
#'
#' # Read in the MSW and variable_details sheet for the PBC model
#' variablesSheet <- read.csv(file.path(getwd(),
#'  'bllFlow/extdata/PBC/PBC - variables.csv'))
#' variableDetailsSheet <- read.csv(file.path(getwd(),
#' 'bllFlow/extdata/PBC/PBC - variable_details.csv'))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' library(bllFlow)
#' pbcModel <- BLLFlow(pbc, variablesSheet, variableDetailsSheet)
#' # passing non dataframe objects when generating the bllFlow object will cause errors
#' #pbcModel <- BLLFlow(pbc, c(1,2,3), variableDetailsSheet)
#'
#' # Clean the data
#' cleanedPbcModel <- clean.Min(pbcModel)
#'
#' # if u wish to be updated in the log on what the function does set print to true
#' cleanedPbcModel <- clean.Min(cleanedPbcModel, print=TRUE)
#'
clean.Min.BLLFlow <- function(bllFlowModel, print = FALSE) {
  bllFlowModel <-
    ProcessMinOrMax(bllFlowModel,
                    pkg.globals$columnNames.Min,
                    print,
                    CheckLessThen)
  
  return(bllFlowModel)
}

# Less then comparing function
CheckLessThen <-
  function(operatorBasedCompareValue,
           valueBeingCompare) {
    return(operatorBasedCompareValue < valueBeingCompare)
  }
#' Clean.Max
#'
#' Clean The data using the Max according to the specifications in the passed object.
#'
#' @param  modelToClean A passed object that contains the data that needs to be cleaned
#' as well as specifications on how to clean it.
#' @param print If set true prints human readable log of the performed cleaning operations
#'
#' @return The passed object with now cleaned data as well as a log containing the performed actions
#' @export
clean.Max <- function(modelToClean, print = FALSE) {
  UseMethod("clean.Max", modelToClean)
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
#' @param print If set to true, prints the following metrics about the model:
#' 1. Number of rows deleted from the dataset
#'
#' @return bllFlowModel that has had its data modified by the paramaters located in
#' the variables object
#' @export
#'
#' @examples
#' # Install the packages
#'
#' # Read in the data we will use
#'
#' library(survival)
#' data(pbc)
#'
#' # Read in the MSW and variable_details sheet for the PBC model
#' variablesSheet <- read.csv(file.path(getwd(),
#'  'bllFlow/extdata/PBC/PBC - variables.csv'))
#' variableDetailsSheet <- read.csv(file.path(getwd(),
#'  'bllFlow/extdata/PBC/PBC - variable_details.csv'))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' library(bllFlow)
#' pbcModel <- BLLFlow(pbc, variablesSheet, variableDetailsSheet)
#' # passing non dataframe objects when generating the bllFlow object will cause errors
#' #pbcModel <- BLLFlow(pbc, c(1,2,3), variableDetailsSheet)
#'
#' # Clean the data
#' cleanedPbcModel <- clean.Max(pbcModel)
#'
#' # if u wish to be updated in the log on what the function does set print to true
#' cleanedPbcModel <- clean.Max(cleanedPbcModel, print=TRUE)
clean.Max.BLLFlow <- function(bllFlowModel, print = FALSE) {
  bllFlowModel <-
    ProcessMinOrMax(bllFlowModel,
                    pkg.globals$columnNames.Max,
                    print,
                    CheckGreaterThen)
  
  return(bllFlowModel)
}

# Greater then comparing function
CheckGreaterThen <-
  function(operatorBasedCompareValue,
           valueBeingCompare) {
    return(operatorBasedCompareValue > valueBeingCompare)
  }

# Internal helper functions ------------------------------------------------------------------
# Function for actually manipulating the data
ProcessMinOrMax <-
  function(bllFlowModel,
           operation,
           print,
           PerformRowCheck) {
    # This is to only store rows which contain instructions for the Operator
    # This is done to avoid parsing through unafected variables
    variablesToCheck <-
      apply(bllFlowModel$variables, 1, function(y)
        if (!is.na(y[operation])) {
          return(list(
            variable = y[["variable"]],
            operation = y[[operation]],
            outlier = y[[pkg.globals$columnNames.Outlier]]
          ))
        })
    # Apply creates list of length of all rows it checks this removes the ones that had no data added
    variablesToCheck[sapply(variablesToCheck, is.null)] <- NULL
    
    # Check if all the variables from variables to check exist in the data
    CheckForExistanceOfInList(variablesToCheck, colnames(bllFlowModel$data))
    
    # Clean the affected rows
    for (variableRowBeingChecked in variablesToCheck) {
      numTotalRows <- nrow(bllFlowModel$data)
      numAffectedRows <- 0
      
      # Does not remove NA rows only less then or greater then
      # Handling for the delete outlier
      if (variableRowBeingChecked[[pkg.globals$columnNames.Outlier]] == "delete") {
        # Remove all rows that pass the rowCheck
        bllFlowModel$data <-
          bllFlowModel$data[!(
            PerformRowCheck(bllFlowModel$data[variableRowBeingChecked$variable], variableRowBeingChecked$operation) &
              !is.na(bllFlowModel$data[variableRowBeingChecked$variable])
          ), ]
        
        numAffectedRows <- numTotalRows - nrow(bllFlowModel$data)
        
        # Handle missing outlier
      } else if (variableRowBeingChecked[[pkg.globals$columnNames.Outlier]] == "missing") {
        # this checks how many rows contained missing before the function was ran to calculate how many were changed
        numPreContainRows <-
          length(which(is.na(bllFlowModel$data[variableRowBeingChecked$variable])))
        bllFlowModel$data[variableRowBeingChecked$variable][PerformRowCheck(bllFlowModel$data[variableRowBeingChecked$variable], variableRowBeingChecked$operation)] <-
          NA
        numPostRows <-
          length(which(is.na(bllFlowModel$data[variableRowBeingChecked$variable])))
        numAffectedRows <- numPostRows - numPreContainRows
        
        # Handle the replace with outlier
      } else if (!is.na(as.numeric(variableRowBeingChecked[[pkg.globals$columnNames.Outlier]]))) {
        # Check how many rows already contained the number that is being changed too to give exact number of changed rows
        numPreContainRows <-
          length(which(bllFlowModel$data[variableRowBeingChecked$variable] == variableRowBeingChecked[[pkg.globals$columnNames.Outlier]]))
        bllFlowModel$data[variableRowBeingChecked$variable][PerformRowCheck(bllFlowModel$data[variableRowBeingChecked$variable], variableRowBeingChecked$operation)] <-
          variableRowBeingChecked[[pkg.globals$columnNames.Outlier]]
        numPostRows <-
          length(which(bllFlowModel$data[variableRowBeingChecked$variable] == variableRowBeingChecked[[pkg.globals$columnNames.Outlier]]))
        numAffectedRows <- numPostRows - numPreContainRows
        
        # Handle non supported outlier
      } else {
        stop(paste("Unsupported outlier method ", variableRowBeingChecked[[pkg.globals$columnNames.Outlier]]))
      }
      
      # Log the the activity of this outlier
      bllFlowModel <-
        LogFunctionActivity(
          bllFlowModel,
          numTotalRows,
          numAffectedRows,
          variableRowBeingChecked[[pkg.globals$columnNames.Outlier]],
          paste(
            variableRowBeingChecked$variable,
            " ",
            operation,
            " at ",
            variableRowBeingChecked$operation,
            sep = ""
          ),
          paste("clean.", operation, ".BLLFlow", sep = ""),
          variableRowBeingChecked$variable,
          variableRowBeingChecked$operation,
          print
        )
    }
    
    return(bllFlowModel)
  }
