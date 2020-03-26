#' Scramble outcome data
#'
#' Scrambles the variables matching the role_name in variables attached to bllflow_object
#' @param bllflow_object a bllflow object containing data variables and variable details
#' @param role_name string matching the variables you wish to scramble
#' @param strata A character vector containing column names you wish to scramble on
#'
#' @return bllflow_object where the variables matching the role are scrambled in working_data
#' @export
scramble_data <- function(bllflow_object, role_name = NULL, strata = NULL) {
  # Extract variables based on role_name values
  vars_to_scramble <-
    as.character(bllflow_object[[pkg.globals$bllFlowContent.Variables]][grepl(role_name, bllflow_object[[pkg.globals$bllFlowContent.Variables]][[pkg.globals$argument.Role]]), pkg.globals$MSW.Variables.Columns.Variable])
  vars_to_scramble <- trimws(vars_to_scramble)
  data_to_scramble <-
    bllflow_object[[pkg.globals$bllFlowContent.WorkingData]]
  
  if (is.null(strata)) {
    scrambled_cols <-
      data_to_scramble[sample(nrow(data_to_scramble)), vars_to_scramble]
    data_to_scramble[, vars_to_scramble] <- scrambled_cols
  } else{
    data_to_scramble <- recurse_scramble(data_to_scramble, strata, vars_to_scramble)
  }
  
  bllflow_object[[pkg.globals$bllFlowContent.WorkingData]] <-
    data_to_scramble
  return(bllflow_object)
}

recurse_scramble <- function(data_subset, strata_vars, vars_to_scramble){
  current_var <- strata_vars[1]
  # Remove first var
  strata_vars <- strata_vars[-1]
  
  # Collect all unique values for current_var
  current_var_values <- unique(data_subset[[current_var]])
  
  # Return Data
  ret_data <- data_subset[0,]
  for (value in current_var_values) {
    data_value_subset <- data_subset[data_subset[[current_var]] == value, ]
    if (length(strata_vars) == 0){
      scrambled_cols <-
        data_value_subset[sample(nrow(data_value_subset)), vars_to_scramble]
      data_value_subset[, vars_to_scramble] <- scrambled_cols
    }else{
      data_value_subset <- recurse_scramble(data_value_subset, strata_vars, vars_to_scramble)
    }
    ret_data <- rbind(ret_data, data_value_subset)
  }
  
  return(ret_data)
}