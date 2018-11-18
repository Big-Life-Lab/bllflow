library(glue)

#' Returns R code to center a variable
#'
#' @param variableNameToCenter The name of the variable to center
#'
#' @return string
#' @export
#'
#' @examples
getCodeForCenteredVariable <- function(variableNameToCenter) {
  centeredVariableName <- glue::glue('{variableNameToCenter}_c')
  
  # The final code looks like:
  # BB6_1_c <- BB6_1 - means[means$Name=="BB6_1_c", ]$Means
  # where uncenteredVariableName = BB6_1_c and centeredVariableName = BB6_1
  # The code makes the folloowing assumptions:
  # 1. There is a variable called means
  # 2. means is a dataframe with a Name and a Means column
  # 3. Name matches with the name of the centeredVariableName var and Means
  #    has the mean for it
  return(glue::glue('{centeredVariableName} <- bllFlow::center({variableNameToCenter}, means[means$Name=="{centeredVariableName}", ]$Means)'))
}