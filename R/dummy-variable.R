library(glue)

getCodeForDummyVariable <- function(dummyVariableName, numberOfCategories) {
  code <- ''
  
  for (i in 1:numberOfCategories) {
    code <- paste(code, glue::glue('{dummyVariableName}_{i-1} <- ifelse({dummyVariableName} == {i}, 1, 0)'), sep="")
  }
  
  return(code)
}