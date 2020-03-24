#' Scramble outcome data
#' 
#' Scrambles the variables matching the role_name in variables attached to bllflow_object
#' @param bllflow_object a bllflow object containing data variables and variable details
#' @param role_name string matching the variables you wish to scramble
#' 
#' @return bllflow_object where the variables matching the role are scrambled in working_data
#' @export
scramble_data <- function(bllflow_object, role_name = NULL){
  # Extract variables based on role_name values
  vars_to_scramble <- as.character(bllflow_object[[pkg.globals$bllFlowContent.Variables]][grepl(role_name, bllflow_object[[pkg.globals$bllFlowContent.Variables]][[pkg.globals$argument.Role]]), pkg.globals$MSW.Variables.Columns.Variable])
  vars_to_scramble <- trimws(vars_to_scramble)
  data_to_scramble <- bllflow_object[[pkg.globals$bllFlowContent.WorkingData]]
  scrambled_cols <- data_to_scramble[sample(nrow(data_to_scramble)), vars_to_scramble]
  data_to_scramble[,vars_to_scramble] <- scrambled_cols
   
  bllflow_object[[pkg.globals$bllFlowContent.WorkingData]] <- data_to_scramble
  
  return(bllflow_object)
}