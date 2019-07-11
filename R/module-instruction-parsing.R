#Parse out the functions inside each module returning a list of the functions in it
ParseModuleFunctions <- function(moduleTable, moduleSequence) {
  #Check row with module sequence
  
  #Seperate each function into seperate list element with 2 elements function name and its arguments
  
}

#Uses the function list to parse out its arguments creating a "function" object
ParseFunctionArguments <- function(functionList) {
  
}

#Uses the function object to find the variables for it and find all the applicable variables
ParseFunctionVariables <-
  function(functionList, variables, variableDetails) {
    
  }

#Uses the function objects to create a recipy
CreateRecipy <- function(functionObjectList, workingData) {
  
}

#verify module sequence matches the passed data
VerifyDataAndSequenceMatch <- function(moduleSequenceNumber, data) {
  if (moduleSequenceNumber == 1 && class(data) == "workingData") {
    stop(
      "Working data was passed when sequance is at step 1. Make sure to pass the starting data.
      Aborting operation!"
    )
  } else if (class(data) != "workingData") {
    stop(
      paste(
        "Non workingData was passed to sequence greater then step 1 please make sure ur passing working data that is result of the module sequence before",
        moduleSequenceNumber,
        "
        Aborting operation!"
      )
      )
  } else if (data[[pkg.globals$WorkingData.ModuleSequenceNumber]] + 1 != moduleSequenceNumber) {
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
    if (moduleSequenceNumber == "all") {
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
        ParseModuleFunctions(moduleTable = modules, moduleSequence = sequenceElement)
      workingData <- CreateRecipy(moduleFunctions, workingData)
    }
    
    return(workingData)
  }