library(glue)

getCodeForDummyVariable <- function(dummyVariableName, numberOfCategories) {
  code <- ''
  
  for (i in 1:numberOfCategories) {
    generatedDummyVariableCode <- glue::glue('{dummyVariableName}_{i-1} <- ifelse({dummyVariableName} == {i}, 1, 0)')
    if(i == 1) {
      code <- glue::glue('{code}{generatedDummyVariableCode}')
    } else {
      code <- glue::glue('{code}\n{generatedDummyVariableCode}')
    }
  }
  
  return(code)
}