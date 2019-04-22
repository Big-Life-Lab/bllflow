#' Creates a DDI object containing the metadata as well as object of overall DDI
#' 
#' Reads DDI from a path to a DDI.xml file it imports variable and value labels as well as
#' all related metadata. Includes study related metadata. The return object is a list of 2
#' containing all variable labels and value labels in first object and second object contains
#' the entire ddi file as a list.
#'
#' @param ddiPath path to the directory containing the ddi file
#' that is used to populate the frame with relevant ddi info
#' @param ddiFile the name of the DDI file
#' @return returns a list containing ddiMetadata and ddi xml parsed object
#' @export
#' @examples 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
ReadDDI <- function(ddiPath, ddiFile) {
  # DDwR crates lots of cat outputs that are suppressed
  ddiMetaData <-
    SuppressFunctionOutput(DDIwR::getMetadata(paste(ddiPath, ddiFile, sep = "/")))
  additionalDDIMetaData <-
    xml2::as_list(xml2::read_xml(paste(ddiPath, ddiFile, sep = "/")))
  
  for (singleVariableIndex in 1:length(additionalDDIMetaData$codeBook$dataDscr)) {
    if (!is.null(attr(additionalDDIMetaData$codeBook$dataDscr[[singleVariableIndex]], "name", exact = TRUE))) {
      varName <-
        attr(additionalDDIMetaData$codeBook$dataDscr[[singleVariableIndex]],
             "name",
             exact = TRUE)
      names(additionalDDIMetaData$codeBook$dataDscr)[singleVariableIndex] <-
        varName
    }
  }
  ddiObject <-
    list(variableMetaData = ddiMetaData, ddiObject = additionalDDIMetaData)
  attr(ddiObject, "class") <-
    c(attr(ddiObject, "class"), "BLLFlowDDI")
  
  return(ddiObject)
}

# Prevents function from writing Cat to console
SuppressFunctionOutput <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' Retrieve docDscr stdyDscr and fileDscr from the DDI
#' 
#' Retrieves only study related information from the DDI object to prevent 
#' clutter of many variables.
#'
#' @param ddi the path to the ddi file containing the necessary information
#' @return returns a list containg he necissary data
#' @export
#' @examples 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
#' 
#' studyDDIHeader <- GetDDIHeader(pbcDDI) 
GetDDIHeader <- function(ddi) {
  ddiObject <- ddi$ddiObject
  additionalDDIMetaData <- list(
    docDscr = ddiObject$codeBook$docDscr,
    stdyDscr = ddiObject$codeBook$stdyDscr,
    fileDscr = ddiObject$codeBook$fileDscr
  )
  
  return(additionalDDIMetaData)
}

#' Creates a CSV file with populated information from DDI
#'
#' @export
WriteDDIPopulatedMSW <- function(x, ...) {
  UseMethod("WriteDDIPopulatedMSW", x)
}

#' Creates a CSV file with populated information from DDI using variables in BLLFlow
#' 
#' Saves the bllFlow model populated variable details to the pathToWriteTo directory
#' under the name of newFileName
#'
#' @param bllFlow Bllflow object containg the variable details to populate
#' @param pathToWriteTo Path to the directory where to write the file
#' @param newFileName the desired name for the recorded csv file
#' @export
#' @examples 
#' library(survival)
#' data(pbc)
#' 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
#' 
#' # read the MSW files
#' variables <- read.csv(file.path(getwd(), 'bllFlow/extdata/PBC-variables.csv'))
#' variableDetails <- read.csv(file.path(getwd(), 'bllFlow/extdata/PBC-variableDetails.csv'))
#' # create a BLLFlow object and add labels.
#' pbcModel <- BLLFlow(pbc, variables, variableDetails, pbcDDI)
#' 
#' WriteDDIPopulatedMSW(pbcModel, "bllFlow/extdata/", "newMSWvariableDetails.csv")
WriteDDIPopulatedMSW.BLLFlow <-
  function(bllFlow, pathToWriteTo, newFileName) {
    
    # create new directory if one does not exist
    if (!dir.exists(pathToWriteTo)) {
      dir.create(file.path(getwd(), pathToWriteTo))
    }
    write.csv(
      bllFlow$populatedVariableDeatailsSheet,
      file = file.path(pathToWriteTo, newFileName),
      row.names = FALSE
    )
  }

#' Creates a CSV file with populated information from DDI using variables
#' in an existing variable details csv
#'
#' @param ddi ddi object containing needed info to get variable details
#' @param pathToMSW path to the directory containg the variable details to read from
#' @param mswName the name of the csv to read the variables from
#' @param newName the desired name of the new csv
#' @export
#' @examples 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
#' 
#' WriteDDIPopulatedMSW(pbcDDI, "bllFlow/extdata/", "PBC-variableDetails.csv", "newName.csv")
WriteDDIPopulatedMSW.BLLFlowDDI <-
  function(ddi, pathToMSW, mswName, newName = NULL) {
    if (!file.exists(file.path(pathToMSW, mswName))) {
      stop(paste("The MSW file is not present in", pathToMSW), call. = FALSE)
    }
    variableDetails <- read.csv(file.path(pathToMSW, mswName))
    populatedVariableDetails <-
      ProcessDDIVariableDetails(ddi, variableDetails)
    
    # create new directory if one does not exist
    if (!dir.exists(pathToMSW)) {
      dir.create(file.path(getwd(), pathToMSW))
    }
    # generate name for new file if one is not provided
    if (is.null(newName)) {
      newName <- paste(mswName, "DDIPopulated.csv", sep = "")
    }
    write.csv(
      populatedVariableDetails,
      file = file.path(pathToMSW, newName),
      row.names = FALSE
    )
  }

#' Gets ddi data related to variables provided
#' 
#' Extracts all ddi related information to provided variable list
#'
#' @param ddi ddi object containing the necessary data
#' @param varList list of variable to retrieve information for
#' @return Returns a list containg data on the variables in varList
#' @export
#' @examples 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
#' 
#' varsForPBC <- GetDDIVariables(pbcDDI, c("age", "sex"))
GetDDIVariables <- function(ddi, varList) {
  retValue <- list()
  requestedVariableIndexes <-
    which(names(ddi$ddiObject$codeBook$dataDscr) %in% varList)
  retValue <-
    ddi$ddiObject$codeBook$dataDscr[requestedVariableIndexes]
  
  return(retValue)
}

#'  Updates the models MSW
#'
#' @param bllModel The model whose MSW needs updating
#' @param newMSWVariables A dataframe containing the new MSWVariables
#' @param newMSWVariableDeatails A dataframe containing the new MSWVariableDetails
#' @return Returns an updated bllFlow model
#' @export
#' @examples 
#' library(survival)
#' data(pbc)
#' 
#' library(bllFlow)
#' 
#' pbcDDI <- ReadDDI(file.path(getwd(), "bllFlow/extdata"), "pbcDDI.xml")
#' 
#' # read the MSW files
#' variables <- read.csv(file.path(getwd(), 'bllFlow/extdata/PBC-variables.csv'))
#' variableDetails <- read.csv(file.path(getwd(), 'bllFlow/extdata/PBC-variableDetails.csv'))
#' # create a BLLFlow object and add labels.
#' pbcModel <- BLLFlow(pbc, variables, variableDetails, pbcDDI)
#' 
#' pbcModel <- UpdateMSW(pbcModel, variables, variableDetails)
#' pbcModel <- UpdateMSW(pbcModel, variables)
#' pbcModel <- UpdateMSW(pbcModel, newMSWVariableDeatails = variableDetails)
UpdateMSW <- function(bllModel, newMSWVariables = NULL, newMSWVariableDeatails = NULL){
  if (!is.null(newMSWVariables)) {
    bllModel[["variables"]] <- newMSWVariables
  }
  if (!is.null(newMSWVariableDeatails)) {
    bllModel[["variableDetails"]] <- newMSWVariableDeatails
    bllModel[["populatedVariableDeatailsSheet"]] <- ProcessDDIVariableDetails(bllModel$ddi, newMSWVariableDeatails)
  }
  
  return(bllModel)
}