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
#' # Test for DDI
#' tmp <- BLLFlow(pbc, variablesSheet, variableDetailsSheet, file.path(getwd(),
#'  'bllFlow/extdata/test-folder/DDI/cchs-82M0013-E-2013-2014-Annual-component.xml')
#'
#' # Passing objects other then a dataframe will create errors
#' #pbcModel <- BLLFlow(pbc, c(1,2,3), list(2,3,4))
BLLFlow <-
  function(data,
           variables,
           variableDetailsSheet,
           ddiXMLMetaData = FALSE) {
    # Verify passed arg integrity for future functions
    CheckIfDataFrame(data, pkg.globals$argument.Data)
    CheckIfDataFrame(variables, pkg.globals$argument.Variables)
    CheckIfDataFrame(variableDetailsSheet,
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
    # if the ddi metadata is supplied populate variable details sheet using it
    if (ddiXMLMetaData != FALSE) {
      variableValueList <- list()
      labeledVariables <- list()
      catStartValue <- numeric()
      catStartLabel <- character()
      variableStartLabel <- character()
      # DDwR crates lots of cat outputs that are suppressed
      ddiMetaData <-
        SupressFunctionCatOutput(DDIwR::getMetadata(ddiXMLMetaData))
      # used for parcing out additional data
      ddiObject <- xml2::as_list(xml2::read_xml(ddiXMLMetaData))
      detectedVariables <-
        unique(variableDetailsSheet[pkg.globals$argument.VariableStart])
      # Find extra info about the variable low and high
      valueForHighLow <- list()
      # Need to loop through every element because the xml2 names all variables var
      for (individualVariable in ddiObject$codeBook$dataDscr) {
        if (!is.null(attr(individualVariable, "name", exact = TRUE))) {
          ddiElementName <- attr(individualVariable, "name", exact = TRUE)
          if (length(detectedVariables[detectedVariables$variableStart == ddiElementName, 1]) != 0) {
            valueForHighLow[[ddiElementName]] <- individualVariable$valrng$range
            valueForHighLow[[ddiElementName]][["Type"]] <-
              ifelse(attr(individualVariable, "intrvl") == "discrete",
                     "cat",
                     "cont")
          }
        }
      }
      
      # Loop through every unique variable found in the VariableDetails
      for (variableToCheck in detectedVariables[, 1]) {
        # Check if that variable is recorded in DDI
        if (variableToCheck %in% names(ddiMetaData$varlab)) {
          # Store the label for that variable
          variableStartLabel <-
            ddiMetaData$varlab[[variableToCheck]]
          variableValueList <- list()
          # Check if that variable has value labels
          if (variableToCheck %in% names(ddiMetaData$vallab)) {
            # Loop Through all the values that are stored for that variable
            for (valueLabelToCheck in names(ddiMetaData$vallab[[variableToCheck]])) {
              catStartValue <-
                ddiMetaData$vallab[[variableToCheck]][[valueLabelToCheck]]
              catStartLabel <- valueLabelToCheck
              # Should i function out the population????
              # Needs to be a character because cant access list with decimal value or zero value
              variableValueList[[as.character(catStartValue)]] <-
                list(
                  variableStartType = valueForHighLow[[variableToCheck]]$Type,
                  catStartValue = catStartValue,
                  catStartLabel = catStartLabel,
                  variableStartLabel = variableStartLabel,
                  variableStartLow = catStartValue,
                  variableStartHigh = catStartValue
                )
            }
            # different values for high and low as well as cat value and label
            if (valueForHighLow[[variableToCheck]]$Type == "cont") {
              variableValueList[[as.character(variableToCheck)]] <- list(
                variableStartType = valueForHighLow[[variableToCheck]]$Type,
                catStartValue = NA,
                catStartLabel = NA,
                variableStartLabel = variableStartLabel,
                variableStartLow = attr(valueForHighLow[[variableToCheck]], "min"),
                variableStartHigh = attr(valueForHighLow[[variableToCheck]], "max")
              )
            }
            # in case there is no labels for the data
          } else{
            variableValueList[[variableToCheck]] <-
              list(
                variableStartType = valueForHighLow[[variableToCheck]]$Type,
                catStartValue = NA,
                catStartLabel = NA,
                variableStartLabel = variableStartLabel,
                variableStartLow = attr(valueForHighLow[[variableToCheck]], "min"),
                variableStartHigh = attr(valueForHighLow[[variableToCheck]], "max")
              )
          }
          # add the list of value labels to that variable
          labeledVariables[[variableToCheck]] <- variableValueList
        }
      }
      if (!length(labeledVariables)) {
        populatedVariableDeatailsSheet <- NULL
      }else{
      populatedVariableDeatailsSheet <-
        PopulateVariableDetails(variableDetailsSheet,
                                labeledVariables,
                                detectedVariables)
      }
      additionalDDIMetaData <- list(
        docDscr = ddiObject$codeBook$docDscr,
        stdyDscr = ddiObject$codeBook$stdyDscr,
        fileDscr = ddiObject$codeBook$fileDscr
      )
    
      } else{
      # Set ddi related vars to null
      additionalDDIMetaData <- NULL
      populatedVariableDeatailsSheet <- NULL
    }
    
    bllFlowModel <-
      list(
        data = data,
        variables = variables,
        variableDetailsSheet = variableDetailsSheet,
        additionalDDIMetaData = additionalDDIMetaData,
        populatedVariableDeatailsSheet = populatedVariableDeatailsSheet,
        ddi <- ddiMetaData
      )
    attr(bllFlowModel, "class") <- "BLLFlow"
    
    return(bllFlowModel)
  }

# Used to populate the dataframe using DDI
PopulateVariableDetails <-
  function(frameToPopulateFrom,
           labeledVariables,
           variablesBeingChanged) {
    # Used to group all the variables in the dataframe
    frameToPopulateFrom <-
      frameToPopulateFrom[order(frameToPopulateFrom$variableStart,
                                frameToPopulateFrom$catStartValue), ]
    onlyDesiredVariables <-
      frameToPopulateFrom[frameToPopulateFrom$variableStart %in% names(labeledVariables),]
    variablesNotRelatedToTheDDI <-
      frameToPopulateFrom[!frameToPopulateFrom$variableStart %in% names(labeledVariables),]
    variablesBeingChanged <-
      unique(onlyDesiredVariables[pkg.globals$argument.VariableStart])
    # Copy all the columns
    finalFrame <- onlyDesiredVariables[0,]
    for (nameIndex in 1:length(names(labeledVariables))) {
      nameBeingChecked <- names(labeledVariables)[[nameIndex]]
      # All the rows for the variable being checked
      rowsToCheck <-
        onlyDesiredVariables[onlyDesiredVariables$variableStart == nameBeingChecked, ]
      # Writes data to relavant rows and removes them from the value object
      for (rowToCheck in 1:nrow(rowsToCheck)) {
        presentCatStartValue <-
          rowsToCheck[rowToCheck, pkg.globals$argument.CatStartValue]
        # Check if the value matches anything in the DDI object
        if (presentCatStartValue %in% names(labeledVariables[[nameBeingChecked]])) {
          # Populate every column with values pulled from DDI
          for (columnName in names(labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]])) {
            if (columnName != pkg.globals$argument.CatStartValue) {
              # Check if there is any data precent in the cell in order to not override anything
              if (CheckIfCellIsEmpty(rowsToCheck[rowToCheck, columnName],
                                     rownames(rowsToCheck)[rowToCheck],
                                     columnName,
                                     labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]][[columnName]])) {
                # If this has not been in the dataframe upon creation that level is added
                if (!labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]][[columnName]] %in% levels(rowsToCheck[, columnName])) {
                  levels(rowsToCheck[, columnName]) <-
                    c(levels(rowsToCheck[, columnName]), labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]][[columnName]])
                }
                rowsToCheck[rowToCheck, columnName] <-
                  labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]][[columnName]]
              }
            }
          }
          # Remove that value from the list to avoid repetition during new row creation
          labeledVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]] <-
            NULL
          finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck,])
        } else{
          # leave the row untouched if no value is matched
          finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck, ])
        }
      }
      # Create new Rows for leftover data
      for (leftOverValue in names(labeledVariables[[nameBeingChecked]])) {
        rowToAdd <-  onlyDesiredVariables[0,]
        for (columnName in names(labeledVariables[[nameBeingChecked]][[leftOverValue]])) {
          if (!labeledVariables[[nameBeingChecked]][[as.character(leftOverValue)]][[columnName]] %in% levels(rowToAdd[, columnName])) {
            levels(rowToAdd[, columnName]) <-
              c(levels(rowToAdd[, columnName]), labeledVariables[[nameBeingChecked]][[as.character(leftOverValue)]][[columnName]])
          }
          rowToAdd[1, columnName] <-
            labeledVariables[[nameBeingChecked]][[leftOverValue]][[columnName]]
        }
        rowToAdd[1, pkg.globals$argument.VariableStart] <-
          nameBeingChecked
        finalFrame <- rbind(finalFrame, rowToAdd)
      }
    }
    finalFrame <- rbind(finalFrame, variablesNotRelatedToTheDDI)
    rownames(finalFrame) <- NULL
    
    return(finalFrame)
  }

# Checks if content of cell is empty and displays appropriate warning
CheckIfCellIsEmpty <-
  function(cellContent,
           rowNumber,
           columnName,
           ddiValue) {
    isEmpty <- TRUE
    if (!is.null(cellContent) &
        !is.na(cellContent) & cellContent != "") {
      warning(
        paste(
          "Row ",
          rowNumber,
          ":",
          columnName,
          " column has value \"",
          cellContent,
          "\" but DDI value is \"",
          ddiValue,
          "\". Not overwriting"
        ),
        call. = FALSE,
        immediate. = TRUE
      )
      isEmpty <- FALSE
    }
    
    return(isEmpty)
  }

# Prevents function from writing Cat to console
SupressFunctionCatOutput <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
