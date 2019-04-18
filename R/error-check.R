CheckForColumnPresence <- function(names, frame, frameName) {
  
  missingColNames <- names[names %in% colnames(frame) == FALSE]
  if (length(missingColNames) != 0) {
    stop(paste(
      "Column(s)",
      missingColNames,
      "are missing from ",
      frameName,
      "\n"
    ))
  }
}

CheckForExistanceOfInList <- function(names, passedList) {
  
  for (name.checkRow in names) {
    if (!(name.checkRow["variable"] %in% passedList)) {
      stop(paste("The ", name.checkRow["variable"], "column is missing from the data"))
    }
  }
}

CheckIfDataFrame <- function(passedFrame, passedName) {
  
  if (!is.data.frame(passedFrame)) {
    stop(paste("The ", passedName, " object is not a data frame"))
  }
}

CheckIfCellIsEmpty <-
  function(cellContent,
           rowNumber,
           columnName,
           ddiValue) {
    isEmpty <- TRUE
    if (!is.null(cellContent) &
        !is.na(cellContent) &
        cellContent != "" & cellContent != ddiValue) {
      warning(
        paste(
          "Row ",
          rowNumber,
          ":",
          columnName,
          " column has value \"",
          cellContent,
          "\" but DDI value is \"",
          ddiValue,
          "\". Not overwriting"
        ),
        call. = FALSE,
        immediate. = TRUE
      )
      isEmpty <- FALSE
    }
    
    return(isEmpty)
  }