parse_function <- function(data,
                           sequence_element,
                           modules,
                           variables,
                           variable_details) {
  working_data <- data
  module_functions <-
    parse_module_functions(
      module_table = modules,
      module_sequence = sequence_element,
      variables = variables,
      variable_details = variable_details
    )
  working_data <-
    create_recipy(module_functions, working_data, variables)

  processed_data <- recipes::bake(working_data, new_data = data)

  return(processed_data)
}

# Use module parsing to find the single column functions???


# And use similar approach as rec custom functions for normal ones??
# if i spot a = then thats a custom par otherwise i get the columns 1 by 1?
# do i use the do again? Slow and inefficient but nice loading bar and hassle free!
# reuse rec_with_table !!!!!!!!!!!!!!!!!!!!!!
