library(DDIwR)
#' Creates a BLLFlow instance from the data arg
#'
#' @param data A dataframe that represents the dataset the model will be developed
#' on
#' @param variables The specification sheet for this model. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=0.
#' CSV file should be read in using the read.csv function.
#' @param variableDetails The variable details worksheet. An example
#' of this worksheet is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036.
#' CSV file should be read in using the read.csv function.
#' @param ddi A string containing the path to the ddi file optional variable if the DDI is present
#' @return Returns a new BllFlow object with the data, variables and
#' variableDetails args attached as variables as well as additional ddiMetadata(Header information) and
#' a populatedVaraiableDetailsSheet that contains information from DDI matching variables in variabledetails.
#' @export
#'
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
#' variableDetails <- read.csv(file.path(getwd(),
#'  'bllFlow/extdata/PBC/PBC - variable_details.csv'))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' library(bllFlow)
#' pbcModel <- BLLFlow(pbc, variablesSheet, variableDetails)
#' # Test for DDI
#' tmp <- BLLFlow(pbc, variablesSheet, variableDetails, file.path(getwd(),
#'  'bllFlow/extdata/test-folder/DDI/cchs-82M0013-E-2013-2014-Annual-component.xml'))
#'
#' # Passing objects other then a dataframe will create errors
#' #pbcModel <- BLLFlow(pbc, c(1,2,3), list(2,3,4))
BLLFlow <-
  function(data,
           variables,
           variableDetails,
           ddi = FALSE) {
    # Verify passed arg integrity for future functions
    CheckIfDataFrame(data, pkg.globals$argument.Data)
    CheckIfDataFrame(variables, pkg.globals$argument.Variables)
    CheckIfDataFrame(variableDetails,
                     pkg.globals$argument.VariableDetailsSheet)
    CheckForColumnPresence(
      c(
        pkg.globals$columnNames.Min,
        pkg.globals$columnNames.Max,
        pkg.globals$columnNames.Outlier
      ),
      variables,
      pkg.globals$argument.Variables
    )
    processedDDI <- ProcessDDI(ddi, variableDetails)
    
    bllFlowModel <-
      list(
        data = data,
        variables = variables,
        variableDetails = variableDetails,
        additionalDDIMetaData = processedDDI[[1]],
        populatedVariableDeatailsSheet = processedDDI[[2]],
        why = processedDDI
      )
    attr(bllFlowModel, "class") <- "BLLFlow"
    
    return(bllFlowModel)
  }




