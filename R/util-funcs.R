# Function to compare even with NA present
# This function returns TRUE wherever elements are the same, including NA's,
# and false everywhere else.
isEqual <- function(v1, v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instanses of NA with FALSE
  same[is.na(same)] <- FALSE
  
  return(same)
}

# Adds the column to the list as well as the dataframe that is passed
AddColumn <-
  function(columnName,
           tableToAddTo) {
    if (!columnName %in% colnames(tableToAddTo)) {
      if (nrow(tableToAddTo) == 0) {
        tableToAddTo[, columnName] <- character()
      } else {
        tableToAddTo[, columnName] <- NA
      }
    }
    
    return(tableToAddTo)
  }

AddGroupByColumns <-
  function(strataSplitName,
           longTable,
           variableDetails) {
    for (groupByIndex in 1:length(strataSplitName)) {
      longTable <-
        AddColumn(paste("groupBy", groupByIndex, sep = ""),
                  longTable)
      longTable <-
        AddColumn(paste("groupByValue", groupByIndex, sep = ""),
                  longTable)
      
      if (!is.null(variableDetails)) {
        longTable <-
          AddColumn(paste("groupByLabel", groupByIndex, sep = ""),
                    longTable)
        longTable <-
          AddColumn(paste("groupByValueLabel", groupByIndex, sep = ""),
                    longTable)
      }
    }
    
    return(longTable)
  }

FillInGroupByColumns <- function(strataSplitName, strataSplitValues, longTableRow, variableDetails) {
  for (groupByIndex in 1:length(strataSplitName)) {
    longTableRow[[paste("groupBy", groupByIndex, sep = "")]] <- strataSplitName[[groupByIndex]]
    longTableRow[[paste("groupByValue", groupByIndex, sep = "")]] <- strataSplitValues[[groupByIndex]]
    
    if (!is.null(variableDetails)) {
      longTableRow[[paste("groupByLabel", groupByIndex, sep = "")]] <- variableDetails[isEqual(variableDetails[[pkg.globals$argument.VariableStart]], strataSplitName[[groupByIndex]]) &
                                                                                         isEqual(variableDetails[[pkg.globals$argument.CatStartValue]], strataSplitValues[[groupByIndex]]), pkg.globals$argument.VariableStartLabel]
      longTableRow[[paste("groupByValueLabel", groupByIndex, sep = "")]] <- variableDetails[isEqual(variableDetails[[pkg.globals$argument.VariableStart]], strataSplitName[[groupByIndex]]) &
                                                                                              isEqual(variableDetails[[pkg.globals$argument.CatStartValue]], strataSplitValues[[groupByIndex]]), pkg.globals$argument.CatStartLabel]
      
    }
  }
  
  return(longTableRow)
}
