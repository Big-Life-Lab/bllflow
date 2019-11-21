# Environment for custom script functions
#' @title Recode with Table
#'
#' @name RecWTable
#'
#' @description \code{RecWTable()} recodes values of variable, where vaiable selection and recoding rules are describe in a reference table (variableDetails). Similar to \code{sjmisc::rec()}. Uses the same rule syntax as \code{sjmisc::rec()}, except rules are in a table as opposed to arguments in the function.
#'
#' @seealso \code{sjmisc::rec()}
#' @export
RecWTable <- function(dataSource = NULL, ...) {
  UseMethod("RecWTable", dataSource)
}

#' @title Recode with Table
#'
#' @name RecWTable.default
#'
#' @details The \code{variableDetails} dataframe has the following variables:
#'  \describe{
#'  \item{variable}{name of new (mutated) variable that is recoded}
#'  \item{databaseStart}{name of dataframe with original variables (\code{variableStart}) to be recoded}
#'  \item{variableStart}{name of variable to be recoded}
#'  \ifem{fromType}{variable type of \code{variableStart}. \code{cat} = categorical or factor variable; \code{cont} = continuous variable (real number or integer)}
#'  \item{catlabel}{label for each category of \code{variable}}
#'  \item{...  add remaining variables (headers) here}
#'  }
#'
#'  Each row in \code{variableDetails} comprises one category in a newly transformed variable. The rules for each category the new variable are a string in \code{recFrom} and value in \code{recTo}. These recode pairs are the same syntax as \code{sjmisc::rec()}, except in \code{sjmisc::rec()} the pairs are a string for the function attibute \code{rec =}, separated by '\code{=}'. For example in \code{RecWTable} \code{variableDetails$recFrom = 2; variableDetails$recTo = 4} is the same as \code{sjmisc::rec(rec = "2=4")}.
#'
#'  the pairs are obtained from the RecFrom and RecTo columns
#'   \describe{
#'     \item{recode pairs}{each recode pair is row. see above example or \code{PBC-variableDetails.csv}
#'     \item{multiple values}{multiple old values that should be recoded into a new single value may be separated with comma, e.g. \code{recFrom = "1,2"; recTo = 1}}
#'     \item{value range}{a value range is indicated by a colon, e.g. \code{recFrom= "1:4"; recTo = 1} (recodes all values from 1 to 4 into 1}
#'     \item{value range for doubles}{for double vectors (with fractional part), all values within the specified range are recoded; e.g. \code{recFrom = "1:2.5'; recTo = 1} recodes 1 to 2.5 into 1, but 2.55 would not be recoded (since it's not included in the specified range)}
#'     \item{\code{"min"} and \code{"max"}}{minimum and maximum values are indicates by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}), e.g. \code{recFrom = "min:4"; recTo = 1} (recodes all values from minimum values of \code{x} to 4 into 1)}
#'     \item{\code{"else"}}{all other values, which have not been specified yet, are indicated by \emph{else}, e.g. \code{recFrom = "else"; recTo = NA} (recode all other values (not specified in other rows) to "NA")}
#'     \item{\code{"copy"}}{the \code{"else"}-token can be combined with \emph{copy}, indicating that all remaining, not yet recoded values should stay the same (are copied from the original value), e.g. \code{recFrom = "else"; recTo = "copy"}
#'     \item{\code{NA}'s}{\code{\link{NA}} values are allowed both as old and new value, e.g. \code{recFrom "NA"; recTo = 1. or "recFrom = "3:5"; recTo = "NA"} (recodes all NA into 1, and all values from 3 to 5 into NA in the new variable)}
#'
#'
#' @param dataSource A dataframe containing the variables to be recoded.
#' @param variableDetails A dataframe containing the specifications (rules) for recoding.
#' @param datasetName String, the name of the dataset containing the variables to be recoded.
#' @param elseValue Value (string, number, integer, logical or NA) that is used to replace any values that are outside the specified ranges (no rules for recoding).
#' @param appendToData Logical, if \code{TRUE} (default), recoded variables will be appended to the dataSource.
#' @param log Logical, if \code{FALSE} (default), a log of recoding will not be printed.
#' @param printNote Logical, if \code{FALSE} (default), will not print the content inside the `Note`` column of the variable beinng recoded.
#' @param appendNonDBColumns Logical, if \code{FALSE} (default), will not append variables if missing in `dataSource`` but present in `variableDetails`.
#' @param variables character vector containing variable names to recode or a variables csv containing additional variable info
#' @param labels named character vector of variable and their label
#'
#' @return a dataframe that is recoded according to rules in variableDetails.
#' @export
RecWTable.default <-
  function(dataSource,
           variableDetails,
           datasetName,
           elseValue = NA,
           appendToData = FALSE,
           log = FALSE,
           printNote = TRUE,
           variables = NULL,
           varLabels = NULL,
           customFunctionPath = NULL) {
    # If custom Functions are passed create new environment and source
    if (!is.null(customFunctionPath)) {
      source(customFunctionPath)
    }
    
    # ---- Step 1: Detemine if the passed data is a list or single database
    appendNonDBColumns <- FALSE
    if (class(dataSource) == "list" &&
        length(datasetName) == length(dataSource)) {
      for (dataName in datasetName) {
        # ---- Step 2A: Verify that the passed name exists in the passed data
        
        if (!is.null(dataSource[[dataName]])) {
          dataSource[[dataName]] <-  RecodeCall(
            variables = variables,
            dataSource = dataSource[[dataName]],
            datasetName = datasetName,
            printNote = printNote,
            elseValue = elseValue,
            variableDetails = variableDetails,
            appendToData = appendToData,
            appendNonDBColumns = appendNonDBColumns,
            log = log,
            varLabels = varLabels
          )
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
      dataSource <-  RecodeCall(
        variables = variables,
        dataSource = dataSource,
        datasetName = datasetName,
        printNote = printNote,
        elseValue = elseValue,
        variableDetails = variableDetails,
        appendToData = appendToData,
        appendNonDBColumns = appendNonDBColumns,
        log = log,
        varLabels = varLabels
      )
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

# Creates inputs and runs recode functions
RecodeCall <-
  function(variables,
           dataSource,
           datasetName,
           printNote,
           elseValue,
           variableDetails,
           appendToData ,
           appendNonDBColumns,
           log,
           varLabels) {
    variableDetails[[pkg.globals$argument.Variables]] <- trimws(variableDetails[[pkg.globals$argument.Variables]])
    if (!is.null(variables) && "data.frame" %in% class(variables)) {
      variableDetails <-
        UpdateVariableDetailsBasedOnVariableSheet(variableSheet = variables, variableDetails = variableDetails)
    } else {
      if (!is.null(variables)) {
        variableDetails <-
          variableDetails[trimws(variableDetails[[pkg.globals$argument.Variables]]) %in% variables, ]
        varsBeingRecoded <-
          as.character(unique(variableDetails[[pkg.globals$argument.Variables]]))
        if (length(varsBeingRecoded) != length(variables)) {
          missingVars <- setdiff(variables, varsBeingRecoded)
          warning(
            paste(missingVars,
            "is missing from variable details therefore cannot be recoded")
          )
        }
      }
      if (is.null(variableDetails[[pkg.globals$argument.VariableLabel]])) {
        variableDetails[[pkg.globals$argument.VariableLabel]] <- NA
      }
      if (is.null(variableDetails[[pkg.globals$argument.VariableLabelShort]])) {
        variableDetails[[pkg.globals$argument.VariableLabelShort]] <- NA
      }
    }
    if (!is.null(varLabels)) {
      if (is.null(names(varLabels))) {
        stop(
          "The passed labels was not a named vector please follow the c(varName = varLalbel) format"
        )
      } else{
        if (is.factor(variableDetails[[pkg.globals$argument.VariableLabelShort]])) {
          variableDetails[[pkg.globals$argument.VariableLabelShort]] <-
            as.character(variableDetails[[pkg.globals$argument.VariableLabelShort]])
        }
        for (varName in names(varLabels)) {
          variableDetails[variableDetails[[pkg.globals$argument.Variables]] == varName, pkg.globals$argument.VariableLabelShort] <-
            varLabels[[varName]]
        }
      }
    }
    
    allPossibleVarNames <-
      unique(as.character(variableDetails[[pkg.globals$argument.Variables]]))
    allVariablesDetected <-
      variableDetails[grepl(datasetName , variableDetails[[pkg.globals$argument.DatabaseStart]]), ]
    
    recData <-
      RecodeColumns(
        dataSource = dataSource,
        variablesToProcess = allVariablesDetected,
        dataName = datasetName,
        log = log,
        printNote = printNote,
        elseDefault = elseValue
      )
    if (appendNonDBColumns) {
      missedVariables <-
        allPossibleVarNames[!allPossibleVarNames %in% unique(as.character(allVariablesDetected[, pkg.globals$argument.Variables]))]
      for (missedVariableName in missedVariables) {
        recData[[missedVariableName]] <- NA
      }
    }
    
    if (appendToData) {
      dataSource <- cbind(dataSource, recData)
    } else{
      dataSource <- recData
    }
    
    return(dataSource)
  }

#' @title Get Data Variable Name
#'
#' @name GetDataVariableName
#'
#' @description Retrieves the name of the column inside dataSource to use for calculations
#'
#' @param dataName name of the database being checked
#' @param rowBeingChecked the row from variable details that contains information on this variables
#' @param variableBeingChecked the name of the recoded variable
#'
#' @return the dataSource equivalant of variableBeingChecked
GetDataVariableName <-
  function(dataName,
           data,
           rowBeingChecked,
           variableBeingChecked) {
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
          "for the variable",
          variableBeingChecked,
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
    
    return(dataVariableBeingChecked)
  }

#' RecodeColumns
#'
#' Recodes columns from passed row returns just table with those columns and same rows as the dataSource
#'
#' @param dataSource The source database
#' @param variablesToProcess rows from variable details that are applicable to this DB
#' @param log The option of printing log
#' @param printNote the option of printing the note columns
#'
#' @return Returns recoded and labeled data
RecodeColumns <-
  function(dataSource,
           variablesToProcess,
           dataName,
           log,
           printNote,
           elseDefault) {
    # Split variables to process into recode map and func
    mapVariablesToProcess <-
      variablesToProcess[grepl("map::", variablesToProcess[[pkg.globals$argument.CatValue]]), ]
    
    funcVariablesToProcess <-
      variablesToProcess[grepl("Func::", variablesToProcess[[pkg.globals$argument.CatValue]]), ]
    
    recVariablesToProcess <-
      variablesToProcess[!grepl("Func::|map::", variablesToProcess[[pkg.globals$argument.CatValue]]), ]
    
    labelList <- list()
    # Set interval if none is present
    intervalPresent <- TRUE
    validIntervals <- c("[,]", "[,)", "(,]")
    intervalDefault <- "[,)"
    recodedData <- dataSource[, 0]
    if (is.null(recVariablesToProcess[[pkg.globals$argument.Interval]])) {
      intervalPresent <- FALSE
    }
    
    # Loop through the rows of recode vars
    while (nrow(recVariablesToProcess) > 0) {
      variableBeingChecked <-
        as.character(recVariablesToProcess[1, pkg.globals$argument.Variables])
      rowsBeingChecked <-
        recVariablesToProcess[recVariablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked, ]
      recVariablesToProcess <-
        recVariablesToProcess[!recVariablesToProcess[[pkg.globals$argument.Variables]] == variableBeingChecked, ]
      firstRow <- rowsBeingChecked[1,]
      # Check for varialbe existance in data
      dataVariableBeingChecked <-
        GetDataVariableName(
          dataName = dataName,
          rowBeingChecked = firstRow,
          variableBeingChecked = variableBeingChecked,
          data = dataSource
        )
      if (is.null(dataSource[[dataVariableBeingChecked]])) {
        warning(
          paste(
            "Data",
            dataName,
            "does not contain the variable",
            dataVariableBeingChecked
          )
        )
      } else{
        # Check for From column duplicates
        allFromValuesForVariable <-
          rowsBeingChecked[[pkg.globals$argument.From]]
        if (length(unique(allFromValuesForVariable)) != length(allFromValuesForVariable)) {
          for (singleFrom in allFromValuesForVariable) {
            if (sum(allFromValuesForVariable == singleFrom) > 1) {
              stop(
                paste(
                  singleFrom,
                  "was detected more then once in",
                  variableBeingChecked,
                  "please make sure only one from value is being recoded"
                )
              )
            }
          }
        }
        
        # Set factor for all recode values
        labelList[[variableBeingChecked]] <-
          CreateLabelListElement(rowsBeingChecked)
        elseValue <-
          as.character(rowsBeingChecked[rowsBeingChecked[[pkg.globals$argument.From]] == "else", pkg.globals$argument.CatValue])
        if (length(elseValue) == 1 &&
            !isEqual(elseValue, "character(0)")) {
          elseValue <-
            RecodeVariableNAFormating(elseValue, labelList[[variableBeingChecked]]$type)
          if (isEqual(elseValue, "copy")) {
            dataVariableBeingChecked <-
              GetDataVariableName(
                dataName = dataName,
                rowBeingChecked = firstRow,
                variableBeingChecked = variableBeingChecked,
                data = dataSource
              )
            recodedData[variableBeingChecked] <-
              dataSource[dataVariableBeingChecked]
          } else {
            recodedData[variableBeingChecked] <- elseValue
          }
          # Catch multiple else rows
        } else if (length(elseValue) > 1) {
          stop(
            paste(
              variableBeingChecked,
              " contains",
              length(elseValue),
              "rows of else only one else value is allowed"
            )
          )
        }
        else{
          recodedData[variableBeingChecked] <- elseDefault
        }
        rowsBeingChecked <-
          rowsBeingChecked[!rowsBeingChecked[[pkg.globals$argument.From]] == "else", ]
        if (nrow(rowsBeingChecked) > 0) {
          logTable <- rowsBeingChecked[, 0]
          logTable$valueTo <- NA
          logTable$From <- NA
          logTable$rowsRecoded <- NA
          levels(recodedData[[variableBeingChecked]]) <-
            c(levels(recodedData[[variableBeingChecked]]), levels(rowsBeingChecked[[pkg.globals$argument.CatValue]]))
          
          for (row in 1:nrow(rowsBeingChecked)) {
            rowBeingChecked <- rowsBeingChecked[row,]
            # If cat go check for label and obtain it
            
            # regardless obtain unit and attach
            
            # find var name for this database
            dataVariableBeingChecked <-
              GetDataVariableName(
                dataName = dataName,
                rowBeingChecked = rowBeingChecked,
                variableBeingChecked = variableBeingChecked,
                data = dataSource
              )
            
            # Recode the variable
            fromValues <- list()
            if (grepl(":", as.character(rowBeingChecked[[pkg.globals$argument.From]]))) {
              fromValues <-
                strsplit(as.character(rowBeingChecked[[pkg.globals$argument.From]]), ":")[[1]]
            } else {
              # TODO find a more elagant way to handle in the future
              tempFrom <-
                as.character(rowBeingChecked[[pkg.globals$argument.From]])
              fromValues[[1]] <- tempFrom
              fromValues[[2]] <- fromValues[[1]]
            }
            valueRecorded <-
              as.character(rowBeingChecked[[pkg.globals$argument.CatValue]])
            if (intervalPresent) {
              interval = as.character(rowBeingChecked[[pkg.globals$argument.Interval]])
              if (!interval %in% validIntervals) {
                interval <- intervalDefault
              }
              if (fromValues[[1]] == fromValues[[2]]) {
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
              } else{
                interval <- intervalDefault
              }
              validRowIndex <- CompareValueBasedOnInterval(
                compareColumns = dataVariableBeingChecked,
                dataSource = dataSource,
                leftBoundary = fromValues[[1]],
                rightBoundary = fromValues[[2]],
                interval = interval
              )
            }
            # Start construction of dataframe for log
            logTable[row, "valueTo"] <- valueRecorded
            logTable[row, "From"] <-
              as.character(rowBeingChecked[[pkg.globals$argument.From]])
            logTable[row, "rowsRecoded"] <-
              sum(validRowIndex, na.rm = TRUE)
            
            valueRecorded <-
              RecodeVariableNAFormating(valueRecorded, labelList[[variableBeingChecked]]$type)
            if (isEqual(valueRecorded, "copy")) {
              valueRecorded <-
                dataSource[validRowIndex, dataVariableBeingChecked]
            }
            recodedData[validRowIndex, variableBeingChecked] <-
              valueRecorded
            if (printNote &&
                !is.null(rowBeingChecked[[pkg.globals$argument.Notes]]) &&
                !isEqual(rowBeingChecked[[pkg.globals$argument.Notes]], "") &&
                !is.na(rowBeingChecked[[pkg.globals$argument.Notes]])) {
              print(paste("NOTE:", as.character(rowBeingChecked[[pkg.globals$argument.Notes]])))
            }
          }
          # if log was requested print it
          if (log) {
            print(
              paste(
                "The variable",
                dataVariableBeingChecked,
                "was recoded into",
                variableBeingChecked,
                "for the database",
                dataName,
                "the following recodes were made:"
              )
            )
            # Reset rowCount to avoid confusion
            rownames(logTable) <- NULL
            print(logTable)
          }
        }
        
      }
    }
    
    # Process funcVars
    while (nrow(funcVariablesToProcess) > 0) {
      firstRow <- funcVariablesToProcess[1, ]
      firstRowVariableName <-
        as.character(firstRow[[pkg.globals$argument.Variables]])
      # get name of var pass to
      derivedReturn <-
        RecodeDerivedVariables(
          variableBeingProcessed = firstRowVariableName,
          recodedData = recodedData,
          variablesToProcess = funcVariablesToProcess,
          log = log,
          printNote = printNote,
          elseDefault = elseDefault,
          labelList = labelList,
          varStack = c()
        )
      labelList <- derivedReturn$labelList
      recodedData <- derivedReturn$recodedData
      funcVariablesToProcess <- derivedReturn$variablesToProcess
    }
    # Populate data Labels
    recodedData <-
      LabelData(labelList = labelList, dataToLabel = recodedData)
    
    return(recodedData)
  }

#' Compare Value Based On Interval
#'
#' Compare values on the scientific notation interval
#'
#' @param leftBoundary the min value
#' @param rightBoundary the max value
#' @param dataSource the data that contains values being compared
#' @param compareColumns The columns inside dataSource being checked
#' @param interval The scientific notation interval
#'
#' @return a boolean vector containing true for rows where the comparison is true
CompareValueBasedOnInterval <-
  function(leftBoundary,
           rightBoundary,
           dataSource,
           compareColumns,
           interval) {
    returnBoolean <- vector()
    if (suppressWarnings(is.na(as.numeric(leftBoundary)))) {
      returnBoolean <-
        dataSource[[compareColumns]] %in% dataSource[[compareColumns]][which(leftBoundary == dataSource[[compareColumns]])]
    } else{
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
    }
    
    return(returnBoolean)
  }

# Parse out variables csv
UpdateVariableDetailsBasedOnVariableSheet <-
  function(variableSheet, variableDetails) {
    # remove conflicting columns from variable details
    variableDetails <-
      variableDetails[,!(
        names(variableDetails) %in% c(
          pkg.globals$MSW.Variables.Columns.VariableType,
          pkg.globals$MSW.Variables.Columns.Label,
          pkg.globals$MSW.Variables.Columns.LabelLong,
          pkg.globals$MSW.Variables.Columns.Units
        )
      )]
    # Only keep the needed columns
    variableSheet <-
      variableSheet[, c(
        pkg.globals$MSW.Variables.Columns.Variable,
        pkg.globals$MSW.Variables.Columns.VariableType,
        pkg.globals$MSW.Variables.Columns.Label,
        pkg.globals$MSW.Variables.Columns.LabelLong,
        pkg.globals$MSW.Variables.Columns.Units
      )]
    # merge the labels and data
    variableDetails <-
      merge(
        variableDetails,
        variableSheet,
        by.x = pkg.globals$argument.Variables,
        by.y = pkg.globals$MSW.Variables.Columns.Variable,
        all.x = TRUE
      )
    # remove variables not present in variableSheet
    variableDetails <-
      variableDetails[variableDetails[[pkg.globals$argument.Variables]] %in% variableSheet[[pkg.globals$MSW.Variables.Columns.Variable]], ]
    
    return(variableDetails)
  }

RecodeVariableNAFormating <- function(cellValue, varType) {
  recodeValue <- NULL
  if (grepl("NA", cellValue)) {
    naValueList <- strsplit(cellValue, ":")[[1]]
    if (isEqual(varType, pkg.globals$argument.CatType)) {
      recodeValue <- paste("NA(", naValueList[[3]], ")", sep = "")
    } else{
      recodeValue <- haven::tagged_na(as.character(naValueList[[3]]))
    }
  } else{
    if (!isEqual(varType, pkg.globals$argument.CatType) &&
        !isEqual(cellValue, "copy")) {
      cellValue <- as.numeric(cellValue)
    }
    recodeValue <- cellValue
  }
  
  return(recodeValue)
}

RecodeDerivedVariables <-
  function(recodedData,
           variableBeingProcessed,
           variablesToProcess,
           log,
           printNote,
           elseDefault,
           labelList,
           varStack) {
    if (nrow(variablesToProcess) <= 0) {
      stop(paste(variableBeingProcessed, "is missing from variableDetails"))
    }
    varStack <- c(varStack, variableBeingProcessed)
    # obtain rows to process and updated variables to Process
    variableRows <-
      variablesToProcess[variablesToProcess[[pkg.globals$argument.Variables]] == variableBeingProcessed, ]
    variablesToProcess <-
      variablesToProcess[variablesToProcess[[pkg.globals$argument.Variables]] != variableBeingProcessed, ]
    for (rowNum in 1:nrow(variableRows)) {
      # Check for presence of feeder variables in data and in the variable being processed stack
      feederVars <-
        as.list(strsplit(as.character(variableRows[rowNum, ][[pkg.globals$argument.VariableStart]]), "::"))[[1]][[2]]
      feederVars <- gsub("\\[|\\]", "", feederVars)
      feederVars <- as.list(strsplit(feederVars, ","))[[1]]
      feederVars <- sapply(feederVars, trimws)
      usedFeederVars <- feederVars
      feederVars <- setdiff(feederVars, names(recodedData))
      
      # Check if the variable has a function to recode
      nonFuncMissingVariables <-
        setdiff(feederVars, unique(as.character(variablesToProcess[[pkg.globals$argument.Variables]])))
      if (length(nonFuncMissingVariables) > 0) {
        warning(
          paste(
            variableBeingProcessed,
            "could not be calculated because",
            feederVars,
            "was never recoded and is not a function variable"
          )
        )
        varStack <- varStack[!(varStack == variableBeingProcessed)]
        
        return(
          list(
            varStack = varStack,
            labelList = labelList,
            recodedData = recodedData,
            variablesToProcess = variablesToProcess
          )
        )
      }
      # Check for presense in varStack
      if (length(intersect(feederVars, varStack)) > 0) {
        conflictVars <- intersect(feederVars, varStack)
        stop(
          paste(
            conflictVars,
            "is required to create",
            variableBeingProcessed,
            "but",
            variableBeingProcessed,
            "is needed to create",
            conflictVars
          )
        )
      }
      
      # Update varStack and recurse to get the feeder vars
      for (oneFeeder in feederVars) {
        # Need to check recoded data again in case a recursion added it
        if (!oneFeeder %in% names(recodedData)) {
          derivedReturn <-
            RecodeDerivedVariables(
              variableBeingProcessed = oneFeeder,
              recodedData = recodedData,
              variablesToProcess = variablesToProcess,
              log = log,
              printNote = printNote,
              elseDefault = elseDefault,
              labelList = labelList,
              varStack = varStack
            )
          varStack <- derivedReturn$varStack
          labelList <- derivedReturn$labelList
          recodedData <- derivedReturn$recodedData
          variablesToProcess <- derivedReturn$variablesToProcess
        }
      }
      
      # Obtain the function for each row
      append(labelList, CreateLabelListElement(variableRows))
      
      rowBeingChecked <- variableRows[rowNum,]
      funcCell <-
        as.character(rowBeingChecked[[pkg.globals$argument.CatValue]])
      functionBeingUsed <-
        as.list(strsplit(funcCell, "::"))[[1]][[2]]
      
      columnValue <-
        recodedData %>% dplyr::rowwise(.) %>% dplyr::select(usedFeederVars) %>%
        dplyr::do(
          columnBeingAdded = CalculateCustomFunctionRowValue(
            .,
            variableNames = usedFeederVars,
            customFunctionName = functionBeingUsed
          )
        )
      recodedData[[variableBeingProcessed]] <-
        unlist(columnValue[["columnBeingAdded"]])
      
      varStack <- varStack[!(varStack == variableBeingProcessed)]
    }
    
    return(
      list(
        varStack = varStack,
        labelList = labelList,
        recodedData = recodedData,
        variablesToProcess = variablesToProcess
      )
    )
    
  }
CalculateCustomFunctionRowValue <-
  function(rowValues,
           variableNames,
           customFunctionName) {
    rowValues <- unname(rowValues)
    # TODO add ability to process ranges in from column
    customFunctionReturnValue <-
      do.call(get(customFunctionName), rowValues)
    
    return(customFunctionReturnValue)
  }
