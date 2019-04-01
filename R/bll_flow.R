library(DDIwR)
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
#' #' # Install the packages
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
#'
#' # Passing objects other then a dataframe will create errors
#' #pbcModel <- BLLFlow(pbc, c(1,2,3), list(2,3,4))
BLLFlow <- function(data, variables, variableDetailsSheet, ddiXMLMetaData = FALSE) {
  # Verify passed arg integrity for future functions
  CheckIfDataFrame(data, "data")
  CheckIfDataFrame(variables, "variables")
  CheckIfDataFrame(variableDetailsSheet, "variableDetailsSheet")
  CheckForColumnPresence(
    c(
      pkg.globals$columnNames.Min,
      pkg.globals$columnNames.Max,
      pkg.globals$columnNames.Outlier
    ),
    variables,
    "variables"
  )
  # if the ddi metadata is supplied populate variable details sheet using it
  if (ddiXMLMetaData != FALSE) {
    ddiMetaData <- DDIwR::getMetadata(ddiXMLMetaData)
    print(ddiMetaData)
  }
  
  bllFlowModel <-
    list(
      data = data,
      variables = variables,
      variableDetailsSheet = variableDetailsSheet
    )
  attr(bllFlowModel, "class") <- "BLLFlow"
  
  return(bllFlowModel)
}
