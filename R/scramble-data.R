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