#' @export
SetDataLabels <-
  function(dataToLabel,
           variableDetails,
           variablesSheet = NULL) {
    # Extract variables in the data
    variableNames <- unique(colnames(dataToLabel))
    # extract only relevant variable info
    if (!is.null(variableDetails)) {
      variableDetails <-
        variableDetails[variableDetails[[pkg.globals$argument.Variables]] %in% variableNames,]
      if (is.null(variablesSheet)){
        variableDetails[[pkg.globals$MSW.Variables.Columns.Label]] <- NA
        variableDetails[[pkg.globals$MSW.Variables.Columns.LabelLong]] <- NA
      }
    }
    if (!is.null(variablesSheet)) {
      variablesSheet <-
        variablesSheet[variablesSheet[[pkg.globals$argument.Variables]] %in% variableNames,]
      variableDetails <- UpdateVariableDetailsBasedOnVariableSheet(variableSheet = variablesSheet, variableDetails = variableDetails)
    }
    labelList <- NULL
    for (variableName in variableNames) {
      rowsToProcess <- variableDetails[variableDetails[[pkg.globals$argument.Variables]] == variableName,]
      labelList[[variableName]] <- CreateLabelListElement(rowsToProcess)
    }
    dataToLabel <- LabelData(labelList, dataToLabel)
    
    return(dataToLabel)
  }

CreateLabelListElement <- function(variableRows) {
  retList <- list(
    type = NULL,
    unit = NULL,
    labelLong = NULL,
    label = NULL,
    values = c(),
    valuesLong = c()
  )
  firstRow <- variableRows[1, ]
  retList$type <-
    as.character(firstRow[[pkg.globals$argument.ToType]])
  retList$unit <-
    as.character(firstRow[[pkg.globals$argument.Units]])
  retList$labelLong <-
    as.character(firstRow[[pkg.globals$argument.VariableLabel]])
  retList$label <-
    as.character(firstRow[[pkg.globals$argument.VariableLabelShort]])
  if (isEqual(retList$type, pkg.globals$ddiValueName.Cat)) {
    for (rowIndex in 1:nrow(variableRows)) {
      singleRow <- variableRows[rowIndex, ]
      # Verify type stays the same
      if (!isEqual(retList$type, as.character(singleRow[[pkg.globals$argument.ToType]]))) {
        stop(
          paste(
            as.character(singleRow[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.ToType,
            "variable cant change variable type for different values"
          )
        )
      }
      # Verify unit is identical
      if (!isEqual(retList$unit, as.character(singleRow[[pkg.globals$argument.Units]]))) {
        stop(
          paste(
            as.character(singleRow[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.Units,
            "variable cant change unit type for different values"
          )
        )
      }
      # Verify variable label is identical
      if (!isEqual(retList$labelLong,
                   as.character(singleRow[[pkg.globals$argument.VariableLabel]]))) {
        stop(
          paste(
            as.character(singleRow[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.VariableLabel,
            "variable cant change variableLabel for different values. VAL1:",
            retList$labelLong,
            "VAL2:",
            as.character(singleRow[[pkg.globals$argument.VariableLabel]])
          )
        )
      }
      valueBeingLabeled <- as.character(singleRow[[pkg.globals$argument.CatValue]])
      valueBeingLabeled <- RecodeVariableNAFormating(valueBeingLabeled, retList$type)
      retList$values[[as.character(singleRow[[pkg.globals$argument.CatLabel]])]] <-
        valueBeingLabeled
      retList$valuesLong[[as.character(singleRow[[pkg.globals$argument.CatLabelLong]])]] <-
        valueBeingLabeled
    }
  }
  
  return(retList)
}

#' LabelData
#'
#' Attaches labels to the DataToLabel to preserve metadata
#'
#' @param labelList the label list object that contains extracted labels from variable details
#' @param dataToLabel The data that is to be labeled
#'
#' @return Returns labeled data
LabelData <- function(labelList, dataToLabel) {
  for (variableName in names(labelList)) {
    if (labelList[[variableName]]$type == pkg.globals$argument.CatType) {
      if (class(dataToLabel[[variableName]]) != "factor") {
        dataToLabel[[variableName]] <- factor(dataToLabel[[variableName]])
      }
      dataToLabel[[variableName]] <-
        sjlabelled::set_labels(dataToLabel[[variableName]], labels = labelList[[variableName]]$values)
      attr(dataToLabel[[variableName]], "labelsLong") <-
        labelList[[variableName]]$valuesLong
    } else{
      if (class(dataToLabel[[variableName]]) == "factor") {
        dataToLabel[[variableName]] <-
          as.numeric(levels(dataToLabel[[variableName]])[dataToLabel[[variableName]]])
      } else{
        dataToLabel[[variableName]] <-
          as.numeric(dataToLabel[[variableName]])
      }
    }
    sjlabelled::set_label(dataToLabel[[variableName]]) <-
      labelList[[variableName]]$label
    attr(dataToLabel[[variableName]], "unit") <-
      labelList[[variableName]]$unit
    attr(dataToLabel[[variableName]], "labelLong") <-
      labelList[[variableName]]$labelLong
  }
  
  return(dataToLabel)
}
