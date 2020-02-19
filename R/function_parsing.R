# ---------- WIP NEEDED FOR FUTURE MODULE UPDATE WITH USER FUNCTION ----------
# parse_function <- function(data,
#                            sequence_element,
#                            modules,
#                            variables,
#                            variable_details) {
#   working_data <- data
#   module_functions <-
#     parse_module_functions(
#       module_table = modules,
#       module_sequence = sequence_element,
#       variables = variables,
#       variable_details = variable_details
#     )
#   working_data <-
#     create_recipy(module_functions, working_data, variables)
#   
#   processed_data <- recipes::bake(working_data, new_data = data)
#   
#   return(processed_data)
# }