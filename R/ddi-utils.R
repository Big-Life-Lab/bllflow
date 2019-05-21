#' Parses a DDI document into an R object
#'
#' Reads the DDI document on a file system and converts it into an R object.
#' Right now the following information is added to the object: \cr
#' 1. Variables info as well as values labels for categorical variables \cr
#' 2. Study Related Metadata
#'
#' @param ddiPath A string containing the path to the directory that has the
#' DDI document
#' @param ddiFile A string containing the name of the DDI document
#' @return A named list which is an instance of a BLLFlowDDI class. The list
#' contains the following members: \cr
#' 1. variableMetaData - A named list. It's value comes from calling the \cr
#' \link[DDIwR]{getMetadata} function \cr
#' 2. additionalDDIMetaData - A named list containig the remaining nodes in the DDI document
#'
#' @export
#' @examples
#' library(bllflow)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
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

#' Parses the headers from a DDI document
#'
#' Retreives the docDscr, stdyDscr and fileDscr nodes from a DDI document, storing
#' them in a named list and returning the list
#'
#' @param ddi A named list created using the \code{\link{ReadDDI}} function
#' @return Returns a named list with the following members: \cr
#' docDscr - Contains the docDscr node in the DDI document \cr
#' stdyDscr - Contains the stdyDscr node in the DDI document \cr
#' fileDscr - Contains the fileDscr node in the DDI document \cr
#'
#' @export
#' @examples
#' library(bllflow)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#'
#' pbcDDIHeaders <- bllflow::GetDDIDescription(pbcDDI)
#' print(names(pbcDDIHeaders))
GetDDIDescription <- function(ddi) {
  ddiObject <- ddi$ddiObject
  additionalDDIMetaData <- list(
    docDscr = ddiObject$codeBook$docDscr,
    stdyDscr = ddiObject$codeBook$stdyDscr,
    fileDscr = ddiObject$codeBook$fileDscr
  )
  
  return(additionalDDIMetaData)
}

#' Writes a variable details CSV sheet to file
#' @param x Object on which we will dispatch
#' @param ... The next method in the chain
#'
#' @export
WriteDDIPopulatedMSW <- function(x, ...) {
  UseMethod("WriteDDIPopulatedMSW", x)
}

#' @describeIn WriteDDIPopulatedMSW The populatedVariableDetails data frame within a bllflow model is written
#' as a CSV file
#'
#' @param pathToWriteTo A string containing the path to the directory
#' where the file should be writtem
#' @param newFileName A string containing the name of the written file
#'
#' @export
#' @examples
#' \dontrun{
#' # Writing the variable details sheet within a bllflow model
#' # _________________________________________________________
#'
#' library(survival)
#' library(bllflow)
#'
#' data(pbc)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#'
#' # Read the MSW files
#' variables <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' variableDetails <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#'
#' # Create a BLLFlow object and add labels.
#' pbcModel <- bllflow::BLLFlow(pbc, variables, variableDetails, pbcDDI)
#'
#'
#' bllflow::WriteDDIPopulatedMSW(pbcModel, "../../inst/extdata/", "newMSWvariableDetails.csv")
#' }
#'
WriteDDIPopulatedMSW.BLLFlow <-
  function(x, pathToWriteTo, newFileName, ...) {
    bllFlow <- x
    
    # create new directory if one does not exist
    if (!dir.exists(pathToWriteTo)) {
      dir.create(file.path(getwd(), pathToWriteTo))
    }
    
    write.csv(
      bllFlow[[pkg.globals$bllFlowContent.PopulatedVariableDetails]],
      file = file.path(pathToWriteTo, newFileName),
      row.names = FALSE
    )
  }

#' @describeIn WriteDDIPopulatedMSW Updates an existing variable details worksheet
#' with metadata from a ddi document and then writes the new variable details
#' sheet to file. The new sheet is saved in the same directory as the old sheet. The
#' first argument should be an object returned by the \code{\link{ReadDDI}} function.
#'
#' @param pathToMSW A string containing the path to the directory with the variable details sheet
#' @param mswName A string containing the name of the variable details sheet
#' @param newName A string containing the name of the new variable details sheet
#'
#' @export
#' @examples
#' \dontrun{
#' # Updating a variable details sheet from file and writing the updated version
#' # ___________________________________________________________________________
#'
#' library(bllflow)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#'
#' bllflow::WriteDDIPopulatedMSW(pbcDDI, "../../inst/extdata/", "PBC-variableDetails.csv", "newName.csv")
#' }
WriteDDIPopulatedMSW.BLLFlowDDI <-
  function(x, pathToMSW, mswName, newName = NULL, ...) {
    ddi <- x
    
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
    
    # generate name for new file name if one is not provided
    if (is.null(newName)) {
      newName <- paste(mswName, "DDIPopulated.csv", sep = "")
    }
    
    write.csv(
      populatedVariableDetails,
      file = file.path(pathToMSW, newName),
      row.names = FALSE
    )
  }

#' Retreives variables in a DDI document
#'
#' Returns a list of dataDscr nodes from a DDI document which represent
#' variables provided in the varList argument
#'
#' @param ddi A named list returned by the \code{\link{ReadDDI}} function
#' @param varList A list of strings that represents variable names
#' @return Returns a list containing the dataDscr nodes from the DDI document,
#' each one of which matches with an entry in the varList argument
#' @export
#' @examples
#' library(bllflow)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#'
#' varsForPBC <- bllflow::GetDDIVariables(pbcDDI, c("age", "sex"))
#' print(attr(varsForPBC[[1]], 'name'))
#' print(varsForPBC[[1]]$labl)
GetDDIVariables <- function(ddi, varList) {
  ddiVariables <- list()
  requestedVariableIndexes <-
    which(names(ddi$ddiObject$codeBook$dataDscr) %in% varList)
  ddiVariables <-
    ddi$ddiObject$codeBook$dataDscr[requestedVariableIndexes]
  
  return(ddiVariables)
}

#' Updates the model specification worksheet (MSW) of a bllflow model. Also updates
#' the variable details sheet with metadata from a ddi document from the original
#' bllflow model if it exists.
#'
#' @param bllModel A bllflow instance whose MSW will be updated
#' @param newMSWVariables A dataframe containing the new MSW variables sheet
#' @param newMSWVariableDeatails A dataframe containing the new MSW variable details sheet
#' @param newDDI A ddi object to add to bllModel
#' @return  bllflow model with it's respective MSW members updated.
#'
#' @export
#' @examples
#' library(survival)
#' library(bllflow)
#'
#' data(pbc)
#'
#' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#'
#' # Read the MSW files
#' variables <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' variableDetails <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#'
#' # Create a BLLFlow object and add labels.
#' pbcModel <- bllflow::BLLFlow(pbc, variables, variableDetails, pbcDDI)
#'
#' pbcModel <- bllflow::UpdateMSW(pbcModel, variables, variableDetails)
#' pbcModel <- bllflow::UpdateMSW(pbcModel, variables)
#' pbcModel <- bllflow::UpdateMSW(pbcModel, newMSWVariableDeatails = variableDetails)
UpdateMSW <-
  function(bllModel,
           newMSWVariables = NULL,
           newMSWVariableDeatails = NULL,
           newDDI = NULL) {
    if (!is.null(newDDI)) {
      bllModel[[pkg.globals$bllFlowContent.DDI]] <- newDDI
      bllModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]] <-
        ProcessDDIVariableDetails(bllModel[[pkg.globals$bllFlowContent.DDI]], bllModel[[pkg.globals$bllFlowContent.VariableDetails]])
    }
    if (!is.null(newMSWVariables)) {
      bllModel[[pkg.globals$bllFlowContent.Variables]] <- newMSWVariables
    }
    if (!is.null(newMSWVariableDeatails)) {
      bllModel[[pkg.globals$bllFlowContent.VariableDetails]] <-
        newMSWVariableDeatails
      if (!is.null(bllModel[[pkg.globals$bllFlowContent.DDI]])) {
        bllModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]] <-
          ProcessDDIVariableDetails(bllModel[[pkg.globals$bllFlowContent.DDI]], newMSWVariableDeatails)
      }
    }
    
    return(bllModel)
  }