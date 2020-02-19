# ---------- DEPRECATED USE MODULES INSTEAD ----------
#' #' Clean a dataset by updating values below a certain minimum
#' #'
#' #' @param bll_flow_model The bllflow model we will clean
#' #' @param ... Arguments to the next method in the chain
#' #'
#' #' @export
#' clean_min <- function(bll_flow_model, ...) {
#'   UseMethod("clean_min")
#' }
#' 
#' #' @describeIn clean_min Cleans the data using the min and outlier columns in the variables sheet of
#' #' the MSW. Outlier method is applied on a row if any of the variable
#' #' values for that row is less than the min value as specified in the variables
#' #' sheet. Outlier checking for the column is not applied if min value is NA.
#' #'
#' #' Currently supported outlier methods are: \cr
#' #' 1. \strong{Delete} - Specified as 'delete' in MSW. Deletes the row from the data. \cr
#' #' number of deleted rows as well as their reason for deletion is stored in the
#' #' metaData variable under the deletedRows name. \cr
#' #' 2. \strong{Missing} - Specified as 'missing' in MSW. Column value for that row which does
#' #' not meet the criteria is set to NA. \cr
#' #' 3. \strong{Not Applicable} - TODO. \cr
#' #' 4. \strong{Set to value} - Specified as a number value in MSW. Column value for the row is
#' #' set to the value specified in the outlier column.
#' #'
#' #' @param print A boolean which when set to TRUE prints logs of what the operation did
#' #'
#' #' @return A bllflow named list whose dataset was cleaned
#' #' @export
#' #'
#' #' @examples
#' #' # Load packages
#' #' library(survival)
#' #' library(bllflow)
#' #'
#' #' # Read in the data we will use
#' #' data(pbc)
#' #'
#' #' # Read in the MSW and variable_details sheet for the PBC model
#' #' variablesSheet <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' #' variableDetailsSheet <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #'
#' #' # Create a bllFlow R object for the PBC model using the above variables as args
#' #' pbcModel <- bllflow::BLLFlow(pbc, variablesSheet, variableDetailsSheet)
#' #'
#' #' # Clean the data
#' #' cleanedPbcModel <- bllflow::clean_min(pbcModel)
#' #'
#' #' # If you wish to be updated in the log on what the function does set print to true
#' #' cleanedPbcModel <- bllflow::clean_min(cleanedPbcModel, print=TRUE)
#' #'
#' clean_min.BLLFlow <- function(bll_flow_model, print = FALSE, ...) {
#'   bll_flow_model <-
#'     process_min_or_max(bll_flow_model,
#'                        pkg.globals$columnNames.Min,
#'                        print,
#'                        check_less_then)
#'   
#'   return(bll_flow_model)
#' }
#' 
#' # Less then comparing function
#' check_less_then <-
#'   function(operator_based_compare_value,
#'            value_being_compare) {
#'     return(operator_based_compare_value < value_being_compare)
#'   }
#' 
#' #' Cleans a dataset by updating values above a certain maximum
#' #'
#' #' @param bll_flow_model The bll_flow_model we will clean
#' #' @param ... Arguments to the next method in the chain
#' #'
#' #' @export
#' clean_max <- function(bll_flow_model, ...) {
#'   UseMethod("clean_max")
#' }
#' 
#' #' @describeIn clean_max Cleans the data using the max and outlier columns in the variables sheet of
#' #' the MSW. Outlier method is applied on a row if any of the variable
#' #' values for that row is greater than the max value as specified in the variables
#' #' sheet. Outlier checking for the column is not applied if max value is NA.
#' #'
#' #' Currently supported outlier methods are: \cr
#' #' \strong{Delete} - Specified as 'delete' in MSW. Deletes the row from the data.
#' #' Deleted rows are stored in the metadata variable under the deletedRows name. \cr
#' #' \strong{Missing} - Specified as 'missing' in MSW. Column value for that row which does
#' #' not meet the criteria is set to NA. \cr
#' #' \strong{Not Applicable} - TODO \cr
#' #' \strong{Set to value} - Specified as a number value in MSW. Column value for the row is
#' #' set to the value specified in the outlier column.
#' #'
#' #' @param print A boolean which when set to TRUE prints logs of what the operation did
#' #'
#' #' @return bll_flow_model that has had its data modified by the paramaters located in
#' #' the variables object
#' #' @export
#' #'
#' #' @examples
#' #' # Load packages
#' #' library(survival)
#' #' library(bllflow)
#' #'
#' #' # Read in the data we will use
#' #' data(pbc)
#' #'
#' #' # Read in the MSW and variable_details sheet for the PBC model
#' #' variablesSheet <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' #' variableDetailsSheet <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #'
#' #' # Create a bllFlow R object for the PBC model using the above variables as args
#' #' pbcModel <- bllflow::BLLFlow(pbc, variablesSheet, variableDetailsSheet)
#' #'
#' #' # Clean the data
#' #' cleanedPbcModel <- bllflow::clean_max(pbcModel)
#' #'
#' #' # If you wish to be updated in the log on what the function does set print to true
#' #' cleanedPbcModel <- bllflow::clean_max(cleanedPbcModel, print=TRUE)
#' #'
#' clean_max.BLLFlow <- function(bll_flow_model, print = FALSE, ...) {
#'   bll_flow_model <-
#'     process_min_or_max(bll_flow_model,
#'                        pkg.globals$columnNames.Max,
#'                        print,
#'                        check_greater_then)
#'   
#'   return(bll_flow_model)
#' }
#' 
#' # Greater then comparing function
#' check_greater_then <-
#'   function(operator_based_compare_value,
#'            value_being_compare) {
#'     return(operator_based_compare_value > value_being_compare)
#'   }
#' 
#' # Internal helper functions ------------------------------------------------------------------
#' # Function for actually manipulating the data
#' process_min_or_max <-
#'   function(bll_flow_model,
#'            operation,
#'            print,
#'            perform_row_check) {
#'     # This is to only store rows which contain instructions for the Operator
#'     # This is done to avoid parsing through unafected variables
#'     variables_to_check <-
#'       apply(bll_flow_model$variables, 1, function(y)
#'         if (!is.na(y[operation])) {
#'           return(list(
#'             variable = y[["variable"]],
#'             operation = y[[operation]],
#'             outlier = y[[pkg.globals$columnNames.Outlier]]
#'           ))
#'         })
#'     # Apply creates list of length of all rows it checks this removes the ones that had no data added
#'     variables_to_check[sapply(variables_to_check, is.null)] <- NULL
#'     
#'     # Check if all the variables from variables to check exist in the data
#'     check_for_existance_of_in_list(variables_to_check, colnames(bll_flow_model$data))
#'     
#'     # Clean the affected rows
#'     for (variable_row_being_checked in variables_to_check) {
#'       num_total_rows <- nrow(bll_flow_model$data)
#'       num_affected_rows <- 0
#'       
#'       # Does not remove NA rows only less then or greater then
#'       # Handling for the delete outlier
#'       if (variable_row_being_checked[[pkg.globals$columnNames.Outlier]] == "delete") {
#'         # Remove all rows that pass the rowCheck
#'         bll_flow_model$data <-
#'           bll_flow_model$data[!(
#'             perform_row_check(
#'               bll_flow_model$data[variable_row_being_checked$variable],
#'               variable_row_being_checked$operation
#'             ) &
#'               !is.na(bll_flow_model$data[variable_row_being_checked$variable])
#'           ),]
#'         
#'         num_affected_rows <-
#'           num_total_rows - nrow(bll_flow_model$data)
#'         
#'         # Handle missing outlier
#'       } else if (variable_row_being_checked[[pkg.globals$columnNames.Outlier]] == "missing") {
#'         # this checks how many rows contained missing before the function was ran to calculate how many were changed
#'         num_pre_contain_rows <-
#'           length(which(is.na(bll_flow_model$data[variable_row_being_checked$variable])))
#'         bll_flow_model$data[variable_row_being_checked$variable][perform_row_check(bll_flow_model$data[variable_row_being_checked$variable],
#'                                                                                    variable_row_being_checked$operation)] <-
#'           NA
#'         num_post_rows <-
#'           length(which(is.na(bll_flow_model$data[variable_row_being_checked$variable])))
#'         num_affected_rows <- num_post_rows - num_pre_contain_rows
#'         
#'         # Handle the replace with outlier
#'       } else if (!is.na(as.numeric(variable_row_being_checked[[pkg.globals$columnNames.Outlier]]))) {
#'         # Check how many rows already contained the number that is being changed too to give exact number of changed rows
#'         num_pre_contain_rows <-
#'           length(which(bll_flow_model$data[variable_row_being_checked$variable] == variable_row_being_checked[[pkg.globals$columnNames.Outlier]]))
#'         bll_flow_model$data[variable_row_being_checked$variable][perform_row_check(bll_flow_model$data[variable_row_being_checked$variable],
#'                                                                                    variable_row_being_checked$operation)] <-
#'           variable_row_being_checked[[pkg.globals$columnNames.Outlier]]
#'         num_post_rows <-
#'           length(which(bll_flow_model$data[variable_row_being_checked$variable] == variable_row_being_checked[[pkg.globals$columnNames.Outlier]]))
#'         num_affected_rows <- num_post_rows - num_pre_contain_rows
#'         
#'         # Handle non supported outlier
#'       } else {
#'         stop(paste(
#'           "Unsupported outlier method ",
#'           variable_row_being_checked[[pkg.globals$columnNames.Outlier]]
#'         ))
#'       }
#'       
#'       # Log the the activity of this outlier
#'       bll_flow_model <-
#'         log_function_activity(
#'           bll_flow_model,
#'           num_total_rows,
#'           num_affected_rows,
#'           variable_row_being_checked[[pkg.globals$columnNames.Outlier]],
#'           paste(
#'             variable_row_being_checked$variable,
#'             " ",
#'             operation,
#'             " at ",
#'             variable_row_being_checked$operation,
#'             sep = ""
#'           ),
#'           paste("clean.", operation, ".BLLFlow", sep = ""),
#'           variable_row_being_checked$variable,
#'           variable_row_being_checked$operation,
#'           print
#'         )
#'     }
#'     
#'     return(bll_flow_model)
#'   }
