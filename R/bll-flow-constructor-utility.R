#' Creates a data frame that holds additional ddi data
#'
#' @param variableDetails The dataframe that contais the variable information
#' that is used to populate the frame with relevant ddi info
#' @param ddiVariable an object that is generated by populateVariables
#' it contains the variables as well as all their value labels and min and max
#' @return returns a dataframe containing new ddi data
PopulateVariableDetails <-
  function(variableDetails,
           ddiVariable) {
    # Used to group all the variables in the dataframe
    variableDetails <-
      variableDetails[order(variableDetails[[pkg.globals$argument.VariableStart]],
                                variableDetails[[pkg.globals$argument.CatStartValue]]),]
    onlyDesiredVariables <-
      variableDetails[variableDetails[[pkg.globals$argument.VariableStart]] %in% names(ddiVariable), ]
    # Copy all the columns
    finalFrame <- onlyDesiredVariables[0, ]
    for (nameIndex in 1:length(names(ddiVariable))) {
      nameBeingChecked <- names(ddiVariable)[[nameIndex]]
      # All the rows for the variable being checked
      rowsToCheck <-
        onlyDesiredVariables[onlyDesiredVariables[[pkg.globals$argument.VariableStart]] == nameBeingChecked,]
      # Writes data to relavant rows and removes them from the value object
      for (rowToCheck in 1:nrow(rowsToCheck)) {
        presentCatStartValue <-
          rowsToCheck[rowToCheck, pkg.globals$argument.CatStartValue]
        # Check if the value matches anything in the DDI object
        if (presentCatStartValue %in% names(ddiVariable[[nameBeingChecked]])) {
          # Populate every column with values pulled from DDI
          selectedVariableCatValue <-
            ddiVariable[[nameBeingChecked]][[as.character(presentCatStartValue)]]
          for (columnName in names(selectedVariableCatValue)) {
            if (columnName != pkg.globals$argument.CatStartValue) {
              # Check if there is any data precent in the cell in order to not override anything
              if (CheckIfCellIsEmpty(
                rowsToCheck[rowToCheck, columnName],
                rownames(rowsToCheck)[rowToCheck],
                columnName,
                selectedVariableCatValue[[columnName]]
              )) {
                # If this has not been in the dataframe upon creation that level is added
                if (!selectedVariableCatValue[[columnName]] %in% levels(rowsToCheck[, columnName])) {
                  levels(rowsToCheck[, columnName]) <-
                    c(levels(rowsToCheck[, columnName]),
                      selectedVariableCatValue[[columnName]])
                }
                rowsToCheck[rowToCheck, columnName] <-
                  selectedVariableCatValue[[columnName]]
              }
            }
          }
          
          # Remove that value from the list to avoid repetition during new row creation
          ddiVariable[[nameBeingChecked]][[as.character(presentCatStartValue)]] <-
            NULL
          finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck, ])
          
        } else if (!is.null(ddiVariable[[nameBeingChecked]][[nameBeingChecked]]) &
                   !is.na(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartType]) &
                   !is.na(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartHigh]) &
                   !is.na(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartLow])) {
          contVariableBeingChecked <-
            ddiVariable[[nameBeingChecked]][[nameBeingChecked]]
          if (rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartHigh] == contVariableBeingChecked[[pkg.globals$argument.VariableStartHigh]] &
              rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartLow] == contVariableBeingChecked[[pkg.globals$argument.VariableStartLow]]) {
            # Populate every column with values pulled from DDI
            for (columnName in names(ddiVariable[[nameBeingChecked]][[as.character(nameBeingChecked)]])) {
              # Check if there is any data precent in the cell in order to not override anything
              if (CheckIfCellIsEmpty(rowsToCheck[rowToCheck, columnName],
                                     rownames(rowsToCheck)[rowToCheck],
                                     columnName,
                                     ddiVariable[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]])) {
                # If this has not been in the dataframe upon creation that level is added
                if (!ddiVariable[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]] %in% levels(rowsToCheck[, columnName])) {
                  levels(rowsToCheck[, columnName]) <-
                    c(levels(rowsToCheck[, columnName]), ddiVariable[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]])
                }
                rowsToCheck[rowToCheck, columnName] <-
                  ddiVariable[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]]
              }
            }
            # Remove that value from the list to avoid repetition during new row creation
            ddiVariable[[nameBeingChecked]][[nameBeingChecked]] <-
              NULL
            finalFrame <-
              rbind(finalFrame, rowsToCheck[rowToCheck, ])
          }
        } else{
      
          # leave the row untouched if no value is matched
          finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck,])
        }
      }
      
      # Create new Rows for leftover data
      for (leftOverValue in names(ddiVariable[[nameBeingChecked]])) {
        rowToAdd <-  onlyDesiredVariables[0, ]
        for (columnName in names(ddiVariable[[nameBeingChecked]][[leftOverValue]])) {
          leftOverVariableValue <-
            ddiVariable[[nameBeingChecked]][[as.character(leftOverValue)]]
          if (!leftOverVariableValue[[columnName]] %in% levels(rowToAdd[, columnName])) {
            levels(rowToAdd[, columnName]) <-
              c(levels(rowToAdd[, columnName]), leftOverVariableValue[[columnName]])
          }
          rowToAdd[1, columnName] <-
            leftOverVariableValue[[columnName]]
        }
        
        rowToAdd[1, pkg.globals$argument.VariableStart] <-
          nameBeingChecked
        finalFrame <- rbind(finalFrame, rowToAdd)
      }
    }
    
    variablesNotRelatedToTheDDI <-
      variableDetails[!variableDetails$variableStart %in% names(ddiVariable), ]
    finalFrame <- rbind(finalFrame, variablesNotRelatedToTheDDI)
    rownames(finalFrame) <- NULL
    
    return(finalFrame)
  }

#' populate variable details sheet using ddi
#'
#' @param ddi the path to the ddi file containing the necessary information
#' @param variableDetails the data frame containing unpopulated variableDetailsSheet
#' @return returns a populated variableDetails dataframe
#' @export
ProcessDDIVariableDetails <- function(ddi, variableDetails) {
  variableValueList <- list()
  ddiVariable <- list()
  variableStartLabel <- character()
  ddiMetaData <- ddi$variableMetaData
  ddiObject <- ddi$ddiObject
  # used for parsing out additional data
  detectedVariables <-
    unique(variableDetails[pkg.globals$argument.VariableStart])
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
            AddDDIToList(
              valueForHighLow[[variableToCheck]]$Type,
              catStartValue,
              catStartLabel,
              variableStartLabel,
              catStartValue,
              catStartValue
            )
        }
        
        # different values for high and low as well as cat value and label
        if (valueForHighLow[[variableToCheck]]$Type == "cont") {
          variableValueList[[as.character(variableToCheck)]] <-
            AddDDIToList(
              valueForHighLow[[variableToCheck]]$Type,
              NA,
              NA,
              variableStartLabel,
              attr(valueForHighLow[[variableToCheck]], "min"),
              attr(valueForHighLow[[variableToCheck]], "max")
            )
        }
        
        # in case there is no labels for the data
      } else{
        variableValueList[[variableToCheck]] <-
          AddDDIToList(
            valueForHighLow[[variableToCheck]]$Type,
            NA,
            NA,
            variableStartLabel,
            attr(valueForHighLow[[variableToCheck]], "min"),
            attr(valueForHighLow[[variableToCheck]], "max")
          )
      }
      # add the list of value labels to that variable
      ddiVariable[[variableToCheck]] <- variableValueList
    }
  }
  
  if (!length(ddiVariable)) {
    populatedVariableDeatailsSheet <- NULL
  } else{
    populatedVariableDeatailsSheet <-
      PopulateVariableDetails(variableDetails,
                              ddiVariable)
  }
  
  return(populatedVariableDeatailsSheet)
}

#' Create ddi variable list
#'
#' @param ddi ddi object containing the necessary data
#' @param varList list of variable to retrieve information for
#' @return Returns a list containg data on the variables in varList
AddDDIToList <- function(variableStartType,
                         catStartValue,
                         catStartLabel,
                         variableStartLabel,
                         variableStartLow ,
                         variableStartHigh) {
  retList <- list()
  retList[[pkg.globals$argument.VariableStartType]] <- variableStartType
  retList[[pkg.globals$argument.CatStartValue]] <- catStartValue
  retList[[pkg.globals$argument.CatStartLabel]] <- catStartLabel
  retList[[pkg.globals$argument.VariableStartLabel]] <- variableStartLabel
  retList[[pkg.globals$argument.VariableStartLow]] <- variableStartLow
  retList[[pkg.globals$argument.VariableStartHigh]] <- variableStartHigh
  
  return(retList)
}
