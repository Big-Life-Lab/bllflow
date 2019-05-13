
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
CreateBLLModelObject <- function(passedData, passedModel, passedTableOne){
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