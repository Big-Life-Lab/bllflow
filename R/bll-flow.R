#' Creates a bllflow model
#' 
#' Wraps up the data, variables and variableDetails arguments in an R object, 
#' making it an instance of a bllflow class and returning the resulting object.
#' If a ddi argument is provided, all the metadata from the DDI document is 
#' imported into the R object
#'
#' @param data A dataframe that represents the dataset the model will be developed
#' on
#' @param variables A dataframe that has the specification sheet for this model. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=0}.
#' @param variableDetails A dataframe that is the variable details worksheet. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036}.
#' @param ddi An optinal string that contains the path to a ddi document
#' @return A named list which is an instance of the bllflow class. The items
#' in the list are specified below: \cr
#' 1. data - A dataframe that contains the passed data argument \cr
#' 2. variables - A dataframe that contains the passed variables argument \cr
#' 3. variableDetails - A dataframe that contains the passed variableDetails argument \cr
#' 4. ddi - A string that contains the passed ddi argument \cr
#' 5. additionalDDIMetaData - A named list. See the return type of the \code{\link{GetDDIDescription}} function \cr
#' 6. populatedVariableDeatails - A dataframe that contains the rows in the variableDetails \cr
#' argument but with additional data filled in using the ddi argument it's specified
#' 
#' @export
#'
#' @examples
#' # ALl the libraries we will be using
#' library(bllflow)
#' library(survival)
#' 
#' # Read in the data we will use for this example
#' data(pbc)
#'
#' # Read in the variables and variable details CSV sheets which are part of the 
#' # master specification workbook
#' variablesSheet <- read.csv(file.path(getwd(),
#'  '../../inst/extdata/PBC-variables.csv'))
#' variableDetails <- read.csv(file.path(getwd(),
#'  '../../inst/extdata/PBC-variableDetails.csv'))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' # and store it in the pbcModel variable
#' pbcModel <- bllflow::BLLFlow(pbc, variablesSheet, variableDetails)
#' 
#' # The pbcModel variable is an R object of instance BLLFlow
#' print(attr(pbcModel, 'class'))
BLLFlow <-
  function(data = NULL,
           variables = NULL,
           variableDetails = NULL,
           ddi = NULL) {
    # Verify passed arg integrity for future functions
    if (!is.null(data)) {
      CheckIfDataFrame(data, pkg.globals$argument.Data)
    }
    if (!is.null(variables)) {
      CheckIfDataFrame(variables, pkg.globals$argument.Variables)
      CheckForColumnPresence(
        c(
          pkg.globals$columnNames.Min,
          pkg.globals$columnNames.Max,
          pkg.globals$columnNames.Outlier
        ),
        variables,
        pkg.globals$argument.Variables
      )
    }
    if (!is.null(variableDetails)) {
      CheckIfDataFrame(variableDetails,
                       pkg.globals$argument.VariableDetailsSheet)
    }
    
    if (!is.null(ddi)) {
      processedVariableDetails <-
        ProcessDDIVariableDetails(ddi, variableDetails)
      ddiHeader <- GetDDIDescription(ddi)
    }else{
      processedVariableDetails <- NULL
      ddiHeader <- NULL
    }
    bllFlowModel <-
      list(
        data = data,
        variables = variables,
        variableDetails = variableDetails,
        additionalDDIMetaData = ddiHeader,
        populatedVariableDeatails = processedVariableDetails,
        ddi = ddi
        
      )
    attr(bllFlowModel, "class") <- "BLLFlow"
    
    return(bllFlowModel)
  }
