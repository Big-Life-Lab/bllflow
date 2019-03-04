CheckForColumnPresence <- function(names, frame, frameName) {
  missingList <- c()
  for (i in names) {
    if (!(i %in% colnames(frame))) {
      missingList <- c(missingList, i)
    }
  }
  if (!is.null(missingList)) {
    stop(paste("The ", missingList, "column is missing from ", frameName, "\n"))
  }
}
CheckForExistanceOfInList <- function(names, passedList) {
  for (i in names) {
    if (!(i[["variable"]] %in% passedList)) {
      stop(paste("The ", i[["variable"]], "column is missing from the data"))
    }
  }
}
CheckIfDataFrame <- function(passedFrame, passedName) {
  if (!is.data.frame(passedFrame)) {
    stop(paste("The ", passedName, " object is not a data frame"))
  }
}
