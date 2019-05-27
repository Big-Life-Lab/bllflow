
CreateReference <- function(x, ...){
  UseMethod("CreateReference")
}

CreateReference.BLLFlow <- function(bllFlowObject, ...){
  
}

CreateReference.default <- function(data, ...){
  
}
  
ExtractData <- function(passedData){
  variableNames <- colnames(passedData)
}

#' @export
CreateBLLModelObject <- function(modelData, modelObject, tableOne = NULL, modelType = NULL) {
  # ----Step 1: verify input/create not passed input----
  supportedModelTypes <- c("crr") 
  varNames <- attr(modelObject$coef, "names")
  
  if (!class(modelObject) %in% supportedModelTypes) {
    stop("Passed model type is not yet supported. Aborting!")
  }
  if (is.null(tableOne)) {
    tableOne <- tableone::CreateTableOne(data = modelData, vars = varNames)
  } else {
    for (varName in varNames) {
      if (!varName %in% tableOne[[pkg.globals$LongTable.MetaData]][[pkg.globals$tableOne.Vars]]) {
       stop("Passed table one does not contain the vars in the passed model. Aborting!") 
      }
    }
  }
  # ----Step 2: Pull out information ----
  
  # ----Step 3: Generate model object ----
  varNames <- attr(modelObject$coef, "names")
  betaCoefficient <- modelObject$coef
  attr(betaCoefficient, "names") <- NULL
  allStrataVarMeans <- list()
  retTable <- data.frame(betaCoefficient = betaCoefficient, row.names = varNames)
  if (!is.null(tableOne$ContTable)) {
    for (strataVar in length(tableOne$ContTable)) {
      allStrataVarMeans[[strataVar]] <-  tableOne$ContTable[[strataVar]][varNames, "mean"]
      attr(allStrataVarMeans[[strataVar]], "names") <- NULL
      retTable[["mean"]] <- allStrataVarMeans[[strataVar]]
    }
  }
  
  return(retTable)
}