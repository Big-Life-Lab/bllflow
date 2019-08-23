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
      retList$values[[as.character(singleRow[[pkg.globals$argument.CatValue]])]] <-
        as.character(singleRow[[pkg.globals$argument.CatLabel]])
      retList$valuesLong[[as.character(singleRow[[pkg.globals$argument.CatValue]])]] <-
        as.character(singleRow[[pkg.globals$argument.CatLabelLong]])
    }
  }
  
  return(retList)
}
