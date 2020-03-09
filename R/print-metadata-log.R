# ---------- WIP TO BE IMPLEMENTED ----------
#' #'Custom print function for the bllFlow metaData log
#' #'
#' #'@param x The metaData log object
#' #'@param ... Arguments for next method in the chain
#' #'
#' #'@export
#' print.metaDataLog <- function(x, ...) {
#'   metaDataLog <- x
#'
#'   print("Data cleaning and trandformation log")
#'   print(paste(length(metaDataLog), "steps performed"))
#'   printDataFrame <-
#'     data.frame(
#'       "Step" = numeric(),
#'       "Function" = character(),
#'       "Variable" = character(),
#'       "Label" = character(),
#'       "Value" = character(),
#'       "Rows" = numeric(),
#'       "Type" = character(),
#'       stringsAsFactors = FALSE
#'     )
#'   for (step in 1:length(metaDataLog)) {
#'     stepRow <-
#'       data.frame(
#'         "Step" = step,
#'         "Function" = metaDataLog[[step]]$fun,
#'         "Variable" = metaDataLog[[step]]$result$variable,
#'         "Label" = metaDataLog[[step]]$result$label,
#'         "Value" = metaDataLog[[step]]$result$value,
#'         "Rows" = metaDataLog[[step]]$result$rowsAffected,
#'         "Type" = metaDataLog[[step]]$result$type
#'       )
#'     printDataFrame <- rbind(printDataFrame, stepRow)
#'   }
#'   print(printDataFrame, row.names = FALSE)
#' }
