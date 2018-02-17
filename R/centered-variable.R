library(glue)

getCodeForCenteredVariable <- function(centeredVariableName) {
  return(glue::glue('{centeredVariableName} <- {centeredVariableName} - means[means$variableName="{centeredVariableName}", ]$Means'))
}