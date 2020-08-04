#' Parse function
#'
#' This function runs modules sequence number on the
#' passed data and returns new data
#'
#' @param data the starting data
#' @param sequence_number the current module sequence number
#' @param modules data.frame containing module information
#' @param variables data.frame containing variable information
#' @param variable_details data.frame containing variable details
#'
#' @return data.frame with processed data
parse_function <- function(data,
                           sequence_number,
                           modules,
                           variables,
                           variable_details) {
  working_data <- data
  module_functions <-
    parse_module_functions(
      module_table = modules,
      module_sequence = sequence_number,
      variables = variables,
      variable_details = variable_details
    )
  working_data <-
    create_recipe(module_functions, working_data, variables)

  processed_data <- recipes::bake(working_data, new_data = data)

  return(processed_data)
}
