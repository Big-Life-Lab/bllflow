
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
#' CreateBLLModelObject (modelObject, modelType, tableOne, modelData)
CreateBLLModelObject <- function(modelData, modelObject, tableOne = NULL, modelType = NULL) {
  # ----Step 1: verify input/create not passed input----
  varNames <- attr(modelObject$coef, "names")
  if (is.null(tableOne)) {
    tableOne <- tableone::CreateTableOne(data = modelData, vars = varNames)
  } else {
    for (varName in varNames) {
      
    }
  }
  # ----Step 2: Pull out information ----
  
  # ----Step 3: Generate model object ----
  varNames <- attr(passedModel$coef, "names")
  betaCoefficient <- passedModel$coef
  attr(betaCoefficient, "names") <- NULL
  allStrataVarMeans <- list()
  retTable <- data.frame(betaCoefficient = betaCoefficient, row.names = varNames)
  if (!is.null(passedTableOne$ContTable)) {
    for (strataVar in length(passedTableOne$ContTable)) {
      allStrataVarMeans[[strataVar]] <-  passedTableOne$ContTable[[strataVar]][varNames, "mean"]
      attr(allStrataVarMeans[[strataVar]], "names") <- NULL
      retTable[["mean"]] <- allStrataVarMeans[[strataVar]]
    }
  }
  
  return(retTable)
}