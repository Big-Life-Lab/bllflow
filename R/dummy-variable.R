library(glue)

#' Returns variable which contains dummying code for a categorical variable
#'
#' @param dummyVariableName The name of the categorical variable
#' @param numberOfCategories The number of categories for the variable
#'
#' @return string
#' @export
#'
#' @examples
getCodeForDummyVariable <- function(dummyVariableName, numberOfCategories) {
  # Will hold the dummying code 
  code <- ''
  
  # For each category....
  for (i in 1:numberOfCategories) {
    # Dummying code for the current category
    # Looks like: BB6_1 <- ifelse(BB6 == 1, 1, 0) assuming dummyVariableName
    # is BB6 and i is 1
    generatedDummyVariableCode <- glue::glue('{dummyVariableName}_{i} <- ifelse({dummyVariableName} == {i}, 1, 0)')
    # If this is the first category then just append the current code to the 
    # overall dummying code
    if (i == 1) {
      code <- glue::glue('{code}{generatedDummyVariableCode}')
    }
    # Otherwise we append the current code to the entire dummying code with
    # a new line between it to format it better
    else {
      code <- glue::glue('{code}\n{generatedDummyVariableCode}')
    }
  }
  
  return(code)
}