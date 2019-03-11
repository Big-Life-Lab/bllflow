#' LogFunctionActivity
#' 
#' A function to insert log information about the caller function
#' as well as print a human readable output to verify caller function activity
#'
#' @param bllFlow BllFlow object containing data and related MetaData
#' @param rowsChecked The amount of rows that the function calling this had to check 
#' @param rowsAffected The amount of rows the caller function changed
#' @param actionTaken What was done with the affected rows
#' @param reason A human readable reason for the action being performed
#' @param executedFunction What was the caller function
#' @param variable What variable the caller function was performed on
#' @param value What was the caller functions compare value
#' @param print Specification if the human readable output needs to be printed
#'
#' @return bllFlow modifiied with the new log data
#' @export
#'
#' @examples
LogFunctionActivity <-
  function(bllFlow,
           rowsChecked,
           rowsAffected,
           actionTaken,
           reason,
           executedFunction,
           variable,
           value,
           print) {
    
    # Print information about the function if the user desires
    if (print) {
      print(
        paste(
          executedFunction,
          ": ",
          rowsChecked,
          " rows were checked and ",
          rowsAffected,
          " rows were set to ",
          actionTaken,
          ". Reason: Rule ",
          reason,
          " was violated",
          sep = ""
        )
      )
    }
    
    # Create a new log if metaData does not yet have a log object
    if (is.null(bllFlow$metaData$log)) {
      bllFlow$metaData$log <- list()
    }
    
    # Populate the log object with data about the function that was executed
    bllFlow$metaData$log <-
      c(bllFlow$metaData$log,
        list(
          fun = executedFunction,
          result = list(
            type = actionTaken,
            rowsAffected = rowsAffected,
            variable = variable,
            value = value
          )
        ))
    
    return(bllFlow)
  }
