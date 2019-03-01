CheckForColumnPresence <- function(names,frame){
  for (i in names) {
    if(is.null(frame[i])){
      stop(paste("The ", i, "column is missing from ", frame))
    }
  }
}
CheckForExistanceOfInList <- function(names,passedList){
  for (i in names) {
    if(is.null(passedList[i]))
  }
  
}