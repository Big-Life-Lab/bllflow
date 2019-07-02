#' @export
RecWTable <- function(dataSource = NULL, ...) {
  UseMethod("RecWTable", dataSource)
}
#' @export
RecWTable.default <-
  function(dataSource,
           variableDetails,
           label = NULL,
           datasetName = NULL,
           elseValue = NA,
           appendToData = TRUE,
           log = TRUE,
           printNote = TRUE,
           predicate = NULL) {
    #---- Step 1: Find all the columns that need to be added by recoding ----
    tmpVariableNames <-
      variableDetails[[pkg.globals$argument.Variables]]
    tmpDataVariableNames <- colnames(dataSource)
    
    # select all variables from variable details that are not in dataSource
    variablesToRecode <-
      unique(tmpVariableNames[!tmpVariableNames %in% tmpDataVariableNames])
    
    # verify that these variables have valid variableStart
    recDataSpecifications <- list()
    for (tmpVerifyVariableName in variablesToRecode) {
      allAffectedRows <-
        variableDetails[variableDetails[[pkg.globals$argument.Variables]] == tmpVerifyVariableName,]
      for (singleRow in 1:nrow(allAffectedRows)) {
        # VariableStart is in the data Proceed as usual
        if (allAffectedRows[singleRow, pkg.globals$argument.VariableStart] %in% tmpDataVariableNames) {
          # This is done to create 1 list element instead of a list wit 4 elements
          appendRecData <- list(
            list(
              varStart = as.character(allAffectedRows[singleRow, pkg.globals$argument.VariableStart]),
              fromValue = as.character(allAffectedRows[singleRow, pkg.globals$argument.From]),
              intervalValue = as.character(allAffectedRows[singleRow, pkg.globals$argument.Interval]),
              value = as.character(allAffectedRows[singleRow, pkg.globals$argument.CatStartValue])
            )
          )
          if (length(recDataSpecifications[[tmpVerifyVariableName]]) == 0) {
            print("Should Be here once")
            recDataSpecifications[[tmpVerifyVariableName]] <-
              appendRecData
          } else{
            print("and here 3 times")
            recDataSpecifications[[tmpVerifyVariableName]] <-
              append(recDataSpecifications[[tmpVerifyVariableName]], appendRecData)
          }
          # VariableStart is a recoded variable Display warning for now
        } else if (allAffectedRows[singleRow, pkg.globals$argument.VariableStart] %in% tmpVariableNames) {
          stop(
            paste(
              "Error: recoding",
              tmpVerifyVariableName,
              "requires",
              allAffectedRows[singleRow, pkg.globals$argument.VariableStart],
              "variable. Recode available in variableDetails. Suggest first recoding",
              allAffectedRows[singleRow, pkg.globals$argument.VariableStart],
              "variable, then try again."
            )
          )
          # VariableStart is not found display error
        } else {
          stop(paste(
            "Error: missing required starting variable(s):",
            allAffectedRows[singleRow, pkg.globals$argument.VariableStart]
          ))
        }
      }
    }
    #TODO add some interval standardization
    #---- Step 2: Creating the Rec vars
    if (appendToData) {
      #---- Step 2A: adding new columns to the data ----
      for (recVariable in variablesToRecode) {
        dataSource[[recVariable]] <- elseValue
        for (recData in recDataSpecifications[[recVariable]]) {
          checkColumn <- recData$varStart
          fromValues <- strsplit(recData$fromValue, ":")[[1]]
          if (!is.na(recData$intervalValue)) {
            validRowIndex <- CompareValueBasedOnInterval(
              compareValue = dataSource[[checkColumn]],
              leftBoundary = fromValues[[1]],
              rightBoundary = fromValues[[2]],
              interval = recData$intervalValue
            )
            dataSource[validRowIndex, recVariable] <- recData$value
          }
        }
      }
    } else{
      #---- Step 2B: Creating new dataFrame with the recVars ----
    }
    
    return(dataSource)
  }

CompareValueBasedOnInterval <-
  function(leftBoundary,
           rightBoundary,
           compareValues,
           interval) {
    returnBoolean <- vector()
    for (compareValue in compareValues) {
      if (interval == "[,]") {
        returnBoolean <-
          c(returnBoolean,
            leftBoundary < compareValue && compareValue > rightBoundary)
      } else if (interval == "[,)") {
        returnBoolean <-
          c(returnBoolean,
            leftBoundary < compareValue && compareValue >= rightBoundary)
      } else if (interval == "(,]") {
        returnBoolean <-
          c(returnBoolean,
            leftBoundary <= compareValue && compareValue > rightBoundary)
      } else{
        stop("Invalid Argument was passed")
      }
    }
    
    return(returnBoolean)
  }