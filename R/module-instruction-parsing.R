#Parse out the functions inside each module returning a list of the functions in it
ParseModuleFunctions <-
  function(moduleTable,
           moduleSequence,
           variables,
           variableDetails) {
    #Check row with module sequence
    rawFunc <-
      as.character(moduleTable[moduleTable[[pkg.globals$Modules.DefaultOrder]] == moduleSequence, pkg.globals$WorkingData.ModuleOperations])
    
    funcList <- strsplit(rawFunc, "],")[[1]]
    #print(funcList)
    
    #Seperate each function into seperate list element with 2 elements function name and its arguments
    refactoredFuncsWithArgs <- list()
    
    for (singleFunc in funcList) {
      funcWithArgs <- as.list(strsplit(singleFunc, "::")[[1]])
      funcWithArgs[[2]] <-
        stringr::str_remove_all(funcWithArgs[[2]], "[\\[\\]]")
      funcWithArgs[[2]] <-
        as.list(strsplit(funcWithArgs[[2]], ",")[[1]])
      
      funcName <- funcWithArgs[[1]]
      refactoredFuncsWithArgs[[funcName]] <- list()
      
      for (argument in funcWithArgs[[2]]) {
        tmpArg <- as.list(strsplit(argument, "=")[[1]])
        tmpArg[[2]] <-
          stringr::str_remove_all(tmpArg[[2]], "[\"\"]")
        refactoredFuncsWithArgs[[funcName]][[pkg.globals$FunctionList.Arguments]][[tmpArg[[1]]]] <-
          tmpArg[[2]]
      }
    }
    
    refactoredFuncsWithArgsAndVars <-
      ParseFunctionVariables(
        functionList = refactoredFuncsWithArgs,
        variables = variables,
        moduleSequenceNumber = moduleSequence
      )
    
    return(refactoredFuncsWithArgsAndVars)
  }

#Uses the function object to find the variables for it and find all the applicable variables
ParseFunctionVariables <-
  function(functionList,
           variables,
           moduleSequenceNumber) {
    #Check which rows contain the module currently being ran
    affectedRows <-
      variables[grepl(moduleSequenceNumber, variables[[pkg.globals$columnNames.Operations]]),]
    
    #Check the additional params for the operations and add to function
    for (currentFuncName in names(functionList)) {
      if (currentFuncName %in% colnames(affectedRows)) {
        columnsToCheck <-
          affectedRows[affectedRows[[currentFuncName]] != FALSE, c(currentFuncName, pkg.globals$columnNames.Variable)]
        
        for (row in 1:nrow(columnsToCheck)) {
          functionList[[currentFuncName]][[pkg.globals$FunctionList.VariableArguments]][[as.character(columnsToCheck[row, pkg.globals$columnNames.Variable])]] <-
            columnsToCheck[row, currentFuncName]
        }
        
      } else{
        warning(
          paste(
            "Requested function",
            currentFuncName,
            "is not present in variables please verify the function name is correct"
          ),
          call. = FALSE
        )
      }
    }
    #Create new functions in case of additional params being there
    functionList <- CreateExactFunction(functionList)
    
    return(functionList)
  }

#Define exact functions depending on variable arguments
CreateExactFunction <- function(functionList) {
  #Parse out non TRUE parameters
  for (funName in names(functionList)) {
    if (!is.null(functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]])) {
      for (varName in names(functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]])) {
        if (functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]] != TRUE) {
          # create new func and move all the ones with that value there
          if (is.null(functionList[[paste(funName, "::", functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]], sep = "")]])) {
            functionList[[paste(funName, "::", functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]], sep = "")]][[pkg.globals$FunctionList.Arguments]] <-
              functionList[[funName]][[pkg.globals$FunctionList.Arguments]]
            functionList[[paste(funName, "::", functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]], sep = "")]][[pkg.globals$FunctionList.VariableArguments]][[varName]] <-
              functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]]
          } else{
            functionList[[paste(funName, "::", functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]], sep = "")]][[pkg.globals$FunctionList.VariableArguments]][[varName]] <-
              functionList[[funName]][[pkg.globals$FunctionList.VariableArguments]][[varName]]
          }
        }
      }
    }
  }
  
  return(functionList)
  #Create new functions with the new paramev ters
}

#Uses the function objects to create a recipy
CreateRecipy <- function(functionObjectList, workingData,variables) {
  # Check variable roles for the recipy creation
  # TODO bllflow support make this add to overall recipy
  outcomeVariable <- as.character(variables[variables[[pkg.globals$argument.Role]] == "outcome",pkg.globals$argument.Variables])
  # TODO add a function for creating a proper formula
  recipeFormula <- paste(outcomeVariable, "~ .")
  
  recipyObject <- recipes::recipe(formula = recipeFormula, data = workingData, x = workingData)
  
  for (singleFunction in names(functionObjectList)) {
    variables <- names(functionObjectList[[singleFunction]][[pkg.globals$FunctionList.VariableArguments]])
    arguments <- functionObjectList[[singleFunction]][[pkg.globals$FunctionList.Arguments]]
    stepFormula <- CreateVariableFormula(variables)
    stepName <- paste("step_",singleFunction, sep = "")
    recipyObject <- do.call(get(stepName),list(recipyObject, stepFormula, arguments))
  }
  recipyObject <- recipes::prep(recipyObject, workingData)
  
  return(recipyObject)
}

#Function for creating formula for variable selection
CreateVariableFormula <- function(varList){
  returnFormula <- character()
  for (variable in varList) {
    returnFormula <- paste(returnFormula, variable, "+")
  }
  returnFormula <- trimws(returnFormula)
  returnFormula <- substr(returnFormula,1,nchar(returnFormula)-2)
  
  return(returnFormula)
}

#verify module sequence matches the passed data
VerifyDataAndSequenceMatch <- function(moduleSequenceNumber, data) {
  if (moduleSequenceNumber[[1]] == 1 &&
      class(data) == "workingData") {
    stop(
      "Working data was passed when sequance is at step 1. Make sure to pass the starting data.
      Aborting operation!"
    )
  } else if (class(data) != "workingData") {
    if (moduleSequenceNumber[[1]] != 1) {
      stop(
        paste(
          "Non workingData was passed to sequence greater then step 1 please make sure ur passing working data that is result of the module sequence before",
          moduleSequenceNumber,
          "
          Aborting operation!"
        )
        )
    }
    } else if (data[[pkg.globals$WorkingData.ModuleSequenceNumber]] + 1 != moduleSequenceNumber[[1]]) {
      stop(
        paste(
          "The WorkingData passed is not from the previous module please verify that the data passed is from module",
          moduleSequenceNumber - 1,
          "
          Aborting operation!"
        )
        )
  }
    }

#' @export
RunModule <-
  function(variables,
           modules,
           data,
           moduleSequenceNumber,
           variableDetails = NULL) {
    #Standardize moduleSequenceNumber
    if (moduleSequenceNumber[[1]] == "all") {
      moduleOrder <- modules[, pkg.globals$Modules.DefaultOrder]
      # Create moduleSequenceNumber out of all default modules
      moduleSequenceNumber <-
        min(moduleOrder, na.rm = TRUE):max(moduleOrder, na.rm = TRUE)
    } else if (!is.numeric(moduleSequenceNumber)) {
      stop(
        "Invalid moduleSequenceNumberPassed please make sure its either the word all or numeric.
        Aborting operation!",
        call. = FALSE
      )
    }
    
    VerifyDataAndSequenceMatch(moduleSequenceNumber, data)
    
    workingData <- data
    for (sequenceElement in moduleSequenceNumber) {
      moduleFunctions <-
        ParseModuleFunctions(
          moduleTable = modules,
          moduleSequence = sequenceElement,
          variables = variables,
          variableDetails = variableDetails
        )
      workingData <- CreateRecipy(moduleFunctions, workingData, variables)
    }
    processedData <- recipes::bake(workingData, data)
    
    return(processedData)
  }