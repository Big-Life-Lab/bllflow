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
            variableDetails[grepl(dataName , variableDetails[["databaseStart"]]),]
          tmpDataVariableNames <- colnames(dataSource[[dataName]])
          variablesToProcess <-
            variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] %in% tmpDataVariableNames,]
          
          # ---- Step 4A: Recode the variables
          recData[[dataName]] <-
            RecodeColumns(
              dataSource = dataSource[[dataName]],
              variablesToProcess = variablesToProcess,
              dataName = dataName
            )
          # ---- Step 5A: Create the output data
          if (appendToData) {
            dataSource[[dataName]] <-
              cbind(dataSource[[dataName]], recData[[dataName]])
          } else{
            dataSource[[dataName]] <- recData[[dataName]]
          }
          
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
      
    } else if ("data.frame" %in% class(dataSource) &&
               length(datasetName) == 1) {
      variablesToProcess <-
        variableDetails[grepl(datasetName , variableDetails[["databaseStart"]]),]
      tmpDataVariableNames <- colnames(dataSource)
      variablesToProcess <-
        variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] %in% tmpDataVariableNames,]
      
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
    validIntervals <- c("[,]", "[,)", "(,]")
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
        variablesToProcess[variablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked,]
      variablesToProcess <-
        variablesToProcess[!variablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked,]
      
      recodedData[variableBeingChecked] <- NA
      for (row in 1:nrow(rowsBeingChecked)) {
        rowBeingChecked <- rowsBeingChecked[row, ]
        # find var name for this database
        dataVariableBeingChecked <- character()
        varStartNames <-
          as.character(rowBeingChecked[[pkg.globals$argument.VariableStart]])
        
        if (grepl(dataName, varStartNames)) {
          varStartNamesList <- as.list(strsplit(varStartNames, ",")[[1]])
          # Find exact var Name
          for (varName in varStartNamesList) {
            if (grepl(dataName, varName)) {
              # seperate dataname from the var name
              dataVariableBeingChecked <-
                as.list(strsplit(varName, "::")[[1]])[[2]]
            }
          }
        } else if (grepl("\\[", varStartNames)) {
          dataVariableBeingChecked <-
            stringr::str_match(varStartNames, "\\[(.*?)\\]")[, 2]
        } else{
          stop(
            paste(
              "The row
              ",
              row,
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
          if (!interval %in% validIntervals) {
            interval <- intervalDefault
          }
          if (fromValues[[1]]==fromValues[[2]]) {
            interval <- "[,]"
          }
          validRowIndex <- CompareValueBasedOnInterval(
            compareColumns = dataVariableBeingChecked,
            dataSource = dataSource,
            leftBoundary = fromValues[[1]],
            rightBoundary = fromValues[[2]],
            interval = interval
          )
        } else{
          if (fromValues[[1]] == fromValues[[2]]) {
            interval <- "[,]"
          }
          validRowIndex <- CompareValueBasedOnInterval(
            compareColumns = dataVariableBeingChecked,
            dataSource = dataSource,
            leftBoundary = fromValues[[1]],
            rightBoundary = fromValues[[2]],
            interval = intervalDefault
          )
        }
        if (is.na(valueRecorded)|| valueRecorded == "N/A") {
          recodedData[validRowIndex, variableBeingChecked] <-
            dataSource[validRowIndex, dataVariableBeingChecked]
        } else{
          recodedData[validRowIndex, variableBeingChecked] <-
            valueRecorded
        }
      }
    }
    
    return(recodedData)
    }

CompareValueBasedOnInterval <-
  function(leftBoundary,
           rightBoundary,
           dataSource,
           compareColumns,
           interval) {
    returnBoolean <- vector()
    if (interval == "[,]") {
      returnBoolean <-
        dataSource[[compareColumns]] %in% dataSource[[compareColumns]][which(
          as.numeric(leftBoundary) <= dataSource[[compareColumns]] &
            dataSource[[compareColumns]] <= as.numeric(rightBoundary)
        )]
    } else if (interval == "[,)") {
      returnBoolean <-
        dataSource[[compareColumns]] %in% dataSource[[compareColumns]][which(
          as.numeric(leftBoundary) <= dataSource[[compareColumns]] &
            dataSource[[compareColumns]] < as.numeric(rightBoundary)
        )]
    } else if (interval == "(,]") {
      returnBoolean <-
        dataSource[[compareColumns]] %in% dataSource[[compareColumns]][which(
          as.numeric(leftBoundary) < dataSource[[compareColumns]] &
            dataSource[[compareColumns]] <= as.numeric(rightBoundary)
        )]
    } else{
      stop("Invalid Argument was passed")
    }
    
    return(returnBoolean)
  }