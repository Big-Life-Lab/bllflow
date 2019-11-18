#' Creates a bllflow model
#'
#' Wraps up the data, variables and variableDetails arguments in an R object,
#' making it an instance of a bllflow class and returning the resulting object.
#' If a ddi argument is provided, all the metadata from the DDI document is
#' imported into the R object
#'
#' @param dataList A dataframe list that represents the datasets the model will be developed
#' on
#' @param variables A dataframe that has the specification sheet for this model. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=0}.
#' @param variableDetails A dataframe that is the variable details worksheet. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036}.
#' @param ddiList A named list that contains the ddi documents
#' @return A named list which is an instance of the bllflow class. The items
#' in the list are specified below: \cr
#' 1. dataList - A dataframe that contains the passed dataList argument \cr
#' 2. variables - A dataframe that contains the passed variables argument \cr
#' 3. variableDetails - A dataframe that contains the passed variableDetails argument \cr
#' 4. ddiList - A named list that contains the ddi found on the passed path \cr
#' 5. additionalDDIMetaData - A named list. See the return type of the \code{\link{GetDDIDescription}} function \cr
#' 6. populatedVariableDetails - A dataframe that contains the rows in the variableDetails \cr
#' argument but with additional data filled in using the ddi argument it's specified
#'
#' @export
#'
#' @examples
#' #TODO Update with lists
#' # ALl the libraries we will be using
#' library(bllflow)
#' library(survival)
#'
#' # Read in the data we will use for this example
#' data(pbc)
#'
#' # Read in the variables and variable details CSV sheets which are part of the
#' # master specification workbook
#' variablesSheet <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' variableDetails <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' # and store it in the pbcModel variable
#' pbcModel <- bllflow::BLLFlow(pbc, variablesSheet, variableDetails)
#'
#' # The pbcModel variable is an R object of instance BLLFlow
#' print(attr(pbcModel, 'class'))
BLLFlow <-
  function(dataList = NULL,
           variables = NULL,
           variableDetails = NULL,
           ddiList = NULL) {
    ddiHeader <- list()
    # Verify passed arg integrity for future functions
    if (!is.null(dataList)) {
      for (singleDataIndex in 1:length(dataList)) {
        CheckIfDataFrame(dataList[[singleDataIndex]], names(dataList)[[singleDataIndex]])
      }
      
    }
    if (!is.null(variables)) {
      CheckIfDataFrame(variables, pkg.globals$argument.Variables)
      # Change the columns needed for the functions
      CheckForColumnPresence(
        c("variable", "label", "labelLong", "variableType", "units"),
        variables,
        pkg.globals$argument.Variables
      )
    }
    if (!is.null(variableDetails)) {
      CheckIfDataFrame(variableDetails,
                       pkg.globals$argument.VariableDetailsSheet)
      CheckForColumnPresence(
        c(
          "variable",
          "toType",
          "databaseStart",
          "variableStart",
          "fromType",
          "recTo",
          "catLabel",
          "catLabelLong",
          "recFrom",
          "units"
        ),
        variableDetails,
        pkg.globals$argument.VariableDetailsSheet
      )
    }
    
    
    if (!is.null(ddiList)) {
      # TODO redisign to create template rather then populate add a check to verify proper structure
      # processedVariableDetails <-
      #   ProcessDDIVariableDetails(ddi, variableDetails)
      for (ddiIndex in 1:length(ddiList)) {
        CheckForExistanceOfInList(c("variableMetaData", "ddiObject"),
                                  ddiList[[ddiIndex]],
                                  paste(names(ddiList)[[ddiIndex]], "ddi"))
        ddiHeader[[names(ddiList)[[ddiIndex]]]] <-
          GetDDIDescription(ddiList[[ddiIndex]])
      }
      
    } else{
      ddiHeader <- NULL
    }
    bllFlowModel <-
      list(
        dataList = dataList,
        variables = variables,
        variableDetails = variableDetails,
        additionalDDIMetaData = ddiHeader,
        populatedVariableDetails = NULL,
        ddiList = ddiList
        
      )
    attr(bllFlowModel, "class") <- "BLLFlow"
    
    return(bllFlowModel)
  }

#' @export
ReadData <- function(variables, dataName, pathToData, nrows = -1) {
  # calculate the rows to set to null
  firstRowOfData <- read.csv(file = pathToData, nrows = 1)
  
  varNamesForThisData <- GetVariables.default(variables, dataName)
  
  columnsToKeep <- colnames(firstRowOfData) %in% varNamesForThisData
  columnClasses <- sapply(columnsToKeep, BooleanConversion)
  
  dataToSave <- read.csv(file = pathToData,
           colClasses = columnClasses, nrows = nrows)
  
  return(dataToSave)
}
BooleanConversion <- function(boolValue) {
  retValue <- character()
  if (boolValue) {
    retValue <- NA
  } else {
    retValue <- "NULL"
  }
  
  return(retValue)
}
#' @export
GetVariables <- function(variableSource = NULL, ...){
  UseMethod("GetVariables", variableSource)
}
#' @export
GetVariables.BLLFlow <- function(bllFlow, dataName){
  variables <- bllFlow[[pkg.globals$bllFlowContent.Variables]]
  
  return(GetVariables.default(variables, dataName))
}
#' @export
GetVariables.default <- function(variables, dataName){
  variablesToReadList <-
    variables[grepl(dataName, variables[[pkg.globals$argument.DatabaseStart]]), ]

  varNamesForThisData <- list()
  
  for (variableToReadRow in 1:nrow(variablesToReadList)) {
    variableToRead <-
      as.character(variablesToReadList[variableToReadRow, pkg.globals$argument.VariableStart])
    dataVariableBeingChecked <- character()
    if (!grepl("DerivedVar", variableToRead)) {
      if (grepl(dataName, variableToRead)) {
        varStartNamesList <- as.list(strsplit(variableToRead, ",")[[1]])
        # Find exact var Name
        for (varName in varStartNamesList) {
          if (grepl(dataName, varName)) {
            # seperate dataname from the var name
            dataVariableBeingChecked <-
              as.list(strsplit(varName, "::")[[1]])[[2]]
          }
        }
      } else if (grepl("\\[", variableToRead)) {
        dataVariableBeingChecked <-
          stringr::str_match(variableToRead, "\\[(.*?)\\]")[, 2]
      }
    }
    
    varNamesForThisData <-
      append(varNamesForThisData, dataVariableBeingChecked)
  }
  varNamesForThisData <- unique(varNamesForThisData)
  
  return(varNamesForThisData)
}
