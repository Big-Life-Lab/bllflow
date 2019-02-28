#' Title
#'
#' @param rowsChecked
#' @param rowsAffected
#' @param actionTaken
#' @param reason
#' @param bllFlow
#'
#' @return
#'
#' @examples
LogFunctionActivity <- function(bllFlow, rowsChecked, rowsAffected, actionTaken, reason, executedFunction, variable, value) {
  print(paste(executedFunction, ": ", rowsChecked, " rows were checked and ", rowsAffected, " rows were set to ", actionTaken, ". Reason: Rule ", reason, " was violated", sep = ""))
  if (is.null(bllFlow$metaData$log)) {
    bllFlow$metaData$log <- list()
  }
  bllFlow$metaData$log <- c(bllFlow$metaData$log, list(fun = executedFunction, result = list(type = actionTaken[["outlier"]], rowsAffected = rowsAffected, variable = variable, value = value)))
  return(bllFlow)
}
