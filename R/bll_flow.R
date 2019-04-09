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
BLLFlow <-
  function(data,
           variables,
           variableDetailsSheet,
           ddiXMLMetaData = FALSE) {
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
      tmpOut <- list()
      labeledVariables <- list()
      standardNAValueLabels <-
        c("NOT STATED", "REFUSAL", "DON'T KNOW", "NOT APPLICABLE")
      variableStartType <- character()
      catStartValue <- numeric()
      catStartLabel <- character()
      variableStartLabel <- character()
      ddiMetaData <- DDIwR::getMetadata(ddiXMLMetaData)
      ddiObject <- xml2::as_list(xml2::read_xml(ddiXMLMetaData))
      detectedVariables <-
        unique(variableDetailsSheet["variableStart"])
      # Find extra info about the variable low and high
      valueForHighLow <- list()
      for (individualVariable in ddiObject$codeBook$dataDscr) {
        if (!is.null(attr(individualVariable, "name"))) {
          ddiElementName <- attr(individualVariable, "name")
          if (length(detectedVariables[detectedVariables$variableStart == ddiElementName, 1]) >
              0) {
            valueForHighLow[[ddiElementName]] <- individualVariable$valrng$range
          }
        }
      }
      populatedPreCursor <- 1
      populatedPostCursor <- 0
      
      # Loop through every unique variable found in the VariableDetails
      for (variableToCheck in detectedVariables[, 1]) {
        # Check if that variable is recorded in DDI
        if (variableToCheck %in% names(ddiMetaData$varlab)) {
          # Store the label for that variable
          variableStartLabel <-
            ddiMetaData$varlab[[variableToCheck]]
          tmpOut <- list()
          # Check if that variable has value labels
          if (variableToCheck %in% names(ddiMetaData$vallab)) {
            # Check if its Continues with labels for NA values
            containsOnlyNAValues <- TRUE
            # Loop Through all the values that are stored for that variable
            
            for (valueLabelToCheck in names(ddiMetaData$vallab[[variableToCheck]])) {
              if (!valueLabelToCheck %in% standardNAValueLabels) {
                containsOnlyNAValues <- FALSE
              }
              catStartValue <-
                ddiMetaData$vallab[[variableToCheck]][[valueLabelToCheck]]
              catStartLabel <- valueLabelToCheck
              # Needs to be a character because cant access list with decimal value or zero value
              tmpOut[[as.character(catStartValue)]] <- list(
                variableStartType = "To Be Determined",
                catStartValue = catStartValue,
                catStartLabel = catStartLabel,
                variableStartLabel = variableStartLabel,
                variableStartLow = attr(valueForHighLow[[variableToCheck]], "min"),
                variableStartHigh = attr(valueForHighLow[[variableToCheck]], "max")
              )
            }
            if (!containsOnlyNAValues) {
              variableStartType <- "Categorical"
            } else{
              variableStartType <- "Continues"
            }
          } else{
            variableStartType <- "Continues"
            tmpOut[[variableToCheck]] <-
              list(
                variableStartType = variableStartType,
                catStartValue = NA,
                catStartLabel = NA,
                variableStartLabel = variableStartLabel,
                variableStartLow = attr(valueForHighLow[[variableToCheck]], "min"),
                variableStartHigh = attr(valueForHighLow[[variableToCheck]], "max")
              )
          }
          for (valueInVariable in names(tmpOut)) {
            tmpOut[[valueInVariable]]$variableStartType <- variableStartType
          }
          labeledVariables[[variableToCheck]] <-
            list(Type = variableStartType, rest = tmpOut)
        }
      }
      finalFrame <- PopulateVariableDetails(variableDetailsSheet,
                              labeledVariables,
                              detectedVariables)
    }
    
    bllFlowModel <-
      list(
        data = data,
        variables = variables,
        variableDetailsSheet = variableDetailsSheet,
        additionalDDIMetaData = list(docDscr = ddiObject$codeBook$docDscr, stdyDscr = ddiObject$codeBook$stdyDscr, fileDscr = ddiObject$codeBook$fileDscr),
        populatedVariableDeatailsSheet = finalFrame
      )
    attr(bllFlowModel, "class") <- "BLLFlow"
    
    return(bllFlowModel)
  }

PopulateVariableDetails <-
  function(frameToPopulateFrom,
           labeledVariables,
           variablesBeingChanged) {
    preCursor <- 1
    frameToPopulateFrom <-
      frameToPopulateFrom[order(frameToPopulateFrom$variableStart,
                                frameToPopulateFrom$catStartValue), ]
    frameToWriteTo <-
      frameToPopulateFrom[frameToPopulateFrom$variableStart %in% names(labeledVariables),]
    rownames(frameToWriteTo) <- NULL
    unEditedData <-
      frameToPopulateFrom[!frameToPopulateFrom$variableStart %in% names(labeledVariables),]
    rownames(unEditedData) <- NULL
    variablesBeingChanged <- unique(frameToWriteTo["variableStart"])
    finalFrame <- frameToWriteTo[0,]
    for (nameIndex in 1:length(names(labeledVariables))) {
      nameBeingChecked <- names(labeledVariables)[[nameIndex]]
      postCursor <-
        rownames(variablesBeingChanged)[which(variablesBeingChanged$variableStart == nameBeingChecked)]
      rowsToCheck <-
        frameToWriteTo[frameToWriteTo$variableStart == nameBeingChecked, ]
      # Writes data to relavant rows and removes them from the value object
      for (rowToCheck in 1:nrow(rowsToCheck)) {
        presentCatStartValue <- rowsToCheck[rowToCheck, "catStartValue"]
        if (presentCatStartValue %in% names(labeledVariables[[nameBeingChecked]]$rest)) {
          for (columnName in names(labeledVariables[[nameBeingChecked]]$rest[[presentCatStartValue]])) {
            if (columnName != "catStartValue") {
              if (CheckIfCellIsEmpty(rowsToCheck[rowToCheck, columnName], rowToCheck, columnName)) {
                rowsToCheck[rowToCheck, columnName] <-
                  labeledVariables[[nameBeingChecked]]$rest[[presentCatStartValue]][[columnName]]
              }
            }
          }
          labeledVariables[[nameBeingChecked]]$rest[[presentCatStartValue]] <- NULL
          #print(rowsToCheck[rowToCheck,])
          # populate row with DDI data related to the value
        }else if (is.na(presentCatStartValue) | is.null(presentCatStartValue)) {
          for (columnName in names(labeledVariables[[nameBeingChecked]]$rest[[1]])) {
              if (CheckIfCellIsEmpty(rowsToCheck[rowToCheck, columnName], rowToCheck, columnName)) {
                rowsToCheck[rowToCheck, columnName] <-
                  labeledVariables[[nameBeingChecked]]$rest[[1]][[columnName]]
              }
          }
          labeledVariables[[nameBeingChecked]]$rest[[1]] <- NULL
          #print(rowsToCheck[rowToCheck,])
        }else{
          # leave the row untouched
          finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck, ])
        }
      }
      finalFrame <- rbind(finalFrame, rowsToCheck)
      
      # Create new Rows for leftover data
      for (leftOverValue in names(labeledVariables[[nameBeingChecked]]$rest)) {
        rowToAdd <-  frameToWriteTo[0,]
        for (columnName in names(labeledVariables[[nameBeingChecked]]$rest[[leftOverValue]])) {
            rowToAdd[1, columnName] <-
              labeledVariables[[nameBeingChecked]]$rest[[leftOverValue]][[columnName]]
        }
        rowToAdd[1, "variableStart"] <- nameBeingChecked
        finalFrame <- rbind(finalFrame,rowToAdd)
      }
    }
    finalFrame <- rbind(finalFrame, unEditedData)
    
    
    return(finalFrame)
    # for (valueBeingRecorded in labeledVariables[[nameBeingChecked]]$rest) {
    #   rowBeingWritten <- frameToWriteTo[preCursor,]
    #   if (rowBeingWritten[[1,"variableStart"]] != nameBeingChecked) {
    #     rowBeingWritten <- frameToPopulateFrom[0,]
    #     rowBeingWritten[1,"variableStartType"] <- labeledVariables[[nameBeingChecked]][["Type"]]
    #     rowBeingWritten[1,"variableStart"] <- nameBeingChecked
    #     rowBeingWritten[1,"catStartValue"] <- valueBeingRecorded[["catStartValue"]]
    #     rowBeingWritten[1,"catStartLabel"] <- valueBeingRecorded[["catStartLabel"]]
    #     rowBeingWritten[1,"variableStartLow"] <- valueBeingRecorded[["variableStartLow"]]
    #     rowBeingWritten[1,"variableStartHigh"] <- valueBeingRecorded[["variableStartHigh"]]
    #     rowBeingWritten[1,"variableStartLabel"] <- valueBeingRecorded[["variableStartLabel"]]
    #   }else{
    #     #check there is anything in value column then fill data accordingly
    #
    #     #check if there is data in any column if there is then create new row
    #     rowBeingWritten <- frameToPopulateFrom[preCursor,]
    #     if (!is.na(rowBeingWritten[1,"catStartValue"])) {
    #       #populate using that value
    #     }else if (!is.na(rowBeingWritten[1,"variableStartType"]) & !is.na(rowBeingWritten[1,"catStartLabel"]) & !is.na(rowBeingWritten[1,"variableStartLow"]) & !is.na(rowBeingWritten[1,"variableStartHigh"]) & !is.na(rowBeingWritten[1,"variableStartLabel"])) {
    #       #populate as regular
    #     }else {
    #       # populate as if fresh row
    #     }
    #     rowBeingWritten[1,"variableStartType"] <- labeledVariables[[nameBeingChecked]][["Type"]]
    #     rowBeingWritten[1,"variableStart"] <- nameBeingChecked
    #     rowBeingWritten[1,"catStartValue"] <- valueBeingRecorded[["catStartValue"]]
    #     rowBeingWritten[1,"catStartLabel"] <- valueBeingRecorded[["catStartLabel"]]
    #     rowBeingWritten[1,"variableStartLow"] <- valueBeingRecorded[["variableStartLow"]]
    #     rowBeingWritten[1,"variableStartHigh"] <- valueBeingRecorded[["variableStartHigh"]]
    #     rowBeingWritten[1,"variableStartLabel"] <- valueBeingRecorded[["variableStartLabel"]]
    #     preCursor <- preCursor+1
    #     if (nameBeingChecked=="WTS_M") {
    #       print(rowBeingWritten)
    #     }
    #   }
    #   #print(rowBeingWritten)
    #
    # }
    
    #print(finalFrame)
    # populatedPostCursor <- rownames(detectedVariables)[which(detectedVariables$variableStart == variableToCheck)]
    # populatedVariableDetailsSheet <- append(populatedVariableDetailsSheet, variableDetailsSheet[populatedPreCursor:populatedPostCursor,])
    # print(populatedVariableDetailsSheet)
    # populatedPreCursor <- populatedPostCursor
    # cat(variableToCheck,"IM IN BOIS", rownames(detectedVariables)[which(detectedVariables$variableStart == variableToCheck)],"\n")
    
  }

CheckIfCellIsEmpty <- function(cellContent, rowNumber, columnName) {
  isEmpty <- TRUE
  if (!is.null(cellContent) & !is.na(cellContent)) {
    warning(
      paste(
        "The cell in row ",
        rowNumber,
        " column ",
        columnName,
        " contains data that is not part of the DDI object ",
        cellContent
      )
    )
    isEmpty <- FALSE
  }
  
  return(isEmpty)
}
