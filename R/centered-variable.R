library(glue)

getCodeForCenteredVariable <- function(uncenteredVariableName, centeredVariableName) {
  return(glue::glue('{centeredVariableName} <- {uncenteredVariableName} - means[means$variableName=="{centeredVariableName}", ]$Means'))
}