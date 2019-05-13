#' @export
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