#' @export
RecWTable <- function(dataSource = NULL, ...) {
  UseMethod("RecWTable", dataSource)
}
#' @export
RecWTable.default <-
  function(dataSource,
           variableDetails,
           datasetName,
           label = NULL,
           elseValue = NA,
           appendToData = TRUE,
           log = TRUE,
           printNote = TRUE,
           predicate = NULL) {
    recData <- list()
    # ---- Step 1: Detemine if the passed data is a list or single database
    
    if (class(dataSource) == "list" &&
        length(datasetName) == length(dataSource)) {
      for (dataName in datasetName) {
        # ---- Step 2A: Verify that the passed name exists in the passed data
        
        if (!is.null(dataSource[[dataName]])) {
          # ---- Step 3A: Extract variables that match this dataSource
          
          variablesToProcess <-
            variableDetails[grepl(dataName , variableDetails[["databaseStart"]]), ]
          tmpDataVariableNames <- colnames(dataSource[[dataName]])
          variablesToProcess <-
            variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] %in% tmpDataVariableNames, ]
          
          recData[[dataName]] <-
            RecodeColumns(
              dataSource = dataSource[[dataName]],
              variablesToProcess = variablesToProcess,
              dataName = dataName
            )
          
          if (appendToData) {
            dataSource[[dataName]] <-
              cbind(dataSource[[dataName]], recData[[dataName]])
          } else{
            dataSource[[dataName]] <- recData[[dataName]]
          }
          # ---- Step 4A:
        } else{
          stop(
            paste(
              "The data",
              dataName,
              "is missing from the passed list please verify the names are correct in the dataSource list and the datasetName list"
            )
          )
        }
      }
      
    } else if (class(dataSource) == "data.frame" &&
               length(datasetName) == 1) {
      variablesToProcess <-
        variableDetails[grepl(datasetName , variableDetails[["databaseStart"]]), ]
      tmpDataVariableNames <- colnames(dataSource)
      variablesToProcess <-
        variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] %in% tmpDataVariableNames, ]
      
      recData[[datasetName]] <-
        RecodeColumns(
          dataSource = dataSource,
          variablesToProcess = variablesToProcess,
          dataName = datasetName
        )
      
      if (appendToData) {
        dataSource <- cbind(dataSource, recData)
      } else{
        dataSource <- recData
      }
    } else{
      stop(
        paste(
          "The passed number of data does not match the passed number of dataNames please verify that the number of databases matches number of passed names.
          Aborting operation!"
        ),
        call. = FALSE
        )
    }
    
    return(dataSource)
  }

# Recodes columns from passed row returns just table with those columns and same rows as the dataSource
RecodeColumns <-
  function(dataSource, variablesToProcess, dataName) {
    # Set interval if none is present
    intervalPresent <- TRUE
    validIntervals <- c("[,]","[,)","(,]")
    intervalDefault <- "[,)"
    recodedData <- dataSource[, 0]
    if (is.null(variablesToProcess[[pkg.globals$argument.Interval]])) {
      intervalPresent <- FALSE
    }
    
    # Loop through the rows
    while (nrow(variablesToProcess) > 0) {
      variableBeingChecked <-
        as.character(variablesToProcess[1, pkg.globals$argument.Variables])
      rowsBeingChecked <-
        variablesToProcess[variablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked, ]
      variablesToProcess <-
        variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked, ]
      
      recodedData[variableBeingChecked] <- NA
      for (row in 1:nrow(rowsBeingChecked)) {
        rowBeingChecked <- rowsBeingChecked[row,]
        # find var name for this database
        varBeingChecked <- character()
        varStartNames <-
          as.character(rowBeingChecked[[pkg.globals$argument.VariableStart]])
        
        if (grepl(dataName, varStartNames)) {
          varStartNamesList <- as.list(strsplit(varStartNames, ",")[[1]])
          # Find exact var Name
          for (varName in varStartNamesList) {
            if (grepl(dataName, varName)) {
              # seperate dataname from the var name
              varBeingChecked <-
                as.list(strsplit(varName, "::")[[1]])[[2]]
            }
          }
        } else if (grepl("\\[", varStartNames)) {
          varBeingChecked <-
            stringr::str_match(varStartNames, "\\[(.*?)\\]")[, 2]
        } else{
          stop(
            paste(
              "The row
              ",
              rowBeingChecked,
              "
              Does not contain the database being checked(",
              dataName,
              ") in its variable start the default is also missing.
              Please double check if this variable should have this",
              dataName,
              "included in its databaseStart"
              )
            )
        }
        
        # Recode the variable
        fromValues <-
          strsplit(as.character(rowBeingChecked[[pkg.globals$argument.From]]), ":")[[1]]
        valueRecorded <-
          rowBeingChecked[[pkg.globals$argument.CatValue]]
        if (intervalPresent) {
          interval = as.character(rowBeingChecked[[pkg.globals$argument.Interval]])
          if (!interval %in% validIntervals){
            interval <- intervalDefault
          }
          validRowIndex <- CompareValueBasedOnInterval(
            compareValue = dataSource[[varBeingChecked]],
            leftBoundary = fromValues[[1]],
            rightBoundary = fromValues[[2]],
            interval = interval
          )
        } else{
          validRowIndex <- CompareValueBasedOnInterval(
            compareValue = dataSource[[varBeingChecked]],
            leftBoundary = fromValues[[1]],
            rightBoundary = fromValues[[2]],
            interval = intervalDefault
          )
        }
        if (is.na(valueRecorded)) {
          valueRecorded <- dataSource[[varBeingChecked]]
        }
        recodedData[validRowIndex, variableBeingChecked] <-
          valueRecorded
        
      }
    }
    
    return(recodedData)
    }

RecodeVariablesForData <- function(dataSource,
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
            leftBoundary < compareValue &&
              compareValue < rightBoundary)
      } else if (interval == "[,)") {
        returnBoolean <-
          c(returnBoolean,
            leftBoundary < compareValue &&
              compareValue <= rightBoundary)
      } else if (interval == "(,]") {
        returnBoolean <-
          c(returnBoolean,
            leftBoundary <= compareValue &&
              compareValue < rightBoundary)
      } else{
        stop("Invalid Argument was passed")
      }
    }
    
    return(returnBoolean)
  }