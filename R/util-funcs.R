# Function to compare even with NA present
# This function returns TRUE wherever elements are the same, including NA's,
# and false everywhere else.
isEqual <- function(v1, v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instanses of NA with FALSE 
  same[is.na(same)] <- FALSE
  
  return(same)
}

# Adds the column to the list as well as the dataframe that is passed
AddColumn <-
  function(columnName,
           valueToSet,
           listToAddTo,
           tableToAddTo) {
    if (length(valueToSet) < 1) {
      valueToSet <- NA
    }
    listToAddTo[[columnName]] <- valueToSet
    if (!columnName %in% colnames(tableToAddTo)) {
      if (nrow(tableToAddTo) == 0) {
        tableToAddTo[[columnName]] <- character()
      } else {
        tableToAddTo[[columnName]] <- NA
      }
    }
    
    return(list(listToAddTo, tableToAddTo))
  }