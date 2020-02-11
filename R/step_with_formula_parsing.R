parse_formula_step <- function(data,
                               sequence_element,
                               modules,
                               variables,
                               variable_details) {
  working_data <- data
  module_functions <-
    parse_formula_module_functions(
      module_table = modules,
      module_sequence = sequence_element,
      variables = variables,
      variable_details = variable_details
    )
  working_data <-
    create_formula_recipy(module_functions, working_data, variables)
  
  processed_data <- recipes::bake(working_data, new_data = data)
  
  return(processed_data)
}

#Parse out the functions inside each module returning a list of the functions in it
parse_formula_module_functions <-
  function(module_table,
           module_sequence,
           variables,
           variable_details) {
    #Check row with module sequence
    raw_func <-
      as.character(module_table[module_table[[pkg.globals$Modules.DefaultOrder]] == module_sequence, pkg.globals$WorkingData.ModuleOperations])
    
    func_list <- strsplit(raw_func, "],")[[1]]
    #print(func_list)
    
    #Seperate each function into seperate list element with 2 elements function name and its arguments
    refactored_funcs_with_args <- list()
    
    for (single_func in func_list) {
      func_with_args <- as.list(strsplit(single_func, "::")[[1]])
      func_with_args[[2]] <-
        stringr::str_remove_all(func_with_args[[2]], "[\\[\\]]")
      func_with_args[[2]] <-
        as.list(strsplit(func_with_args[[2]], ",")[[1]])
      
      func_name <- func_with_args[[1]]
      refactored_funcs_with_args[[func_name]] <- list()
      
      for (argument in func_with_args[[2]]) {
        tmp_arg <- as.list(strsplit(argument, "}")[[1]])
        tmp_arg[[1]] <-
                 stringr::str_remove_all(tmp_arg[[1]], "[{}]")
        refactored_funcs_with_args[[func_name]][[pkg.globals$FunctionList.Arguments]][[tmp_arg[[1]]]] <-
          tmp_arg[[2]]
      }
    }
      refactored_funcs_with_args_and_vars <-
        parse_formula_function_variables(
          function_list = refactored_funcs_with_args,
          variables = variables,
          module_sequence_number = module_sequence
        )

    return(refactored_funcs_with_args_and_vars)
  }

#Uses the function object to find the variables for it and find all the applicable variables
parse_formula_function_variables <-
  function(function_list,
           variables,
           module_sequence_number) {
    #Check which rows contain the module currently being ran
    affected_rows <-
      variables[grepl(module_sequence_number, variables[[pkg.globals$columnNames.Operations]]), ]
    
    #Check the additional params for the operations and add to function
    for (current_func_name in names(function_list)) {
      if (current_func_name %in% colnames(affected_rows)) {
        columns_to_check <-
          affected_rows[affected_rows[[current_func_name]] != FALSE, c(current_func_name, pkg.globals$columnNames.Variable)]
        
        for (row in 1:nrow(columns_to_check)) {
          function_list[[current_func_name]][[pkg.globals$FunctionList.VariableArguments]][[as.character(columns_to_check[row, pkg.globals$columnNames.Variable])]] <-
            columns_to_check[row, current_func_name]
        }
        
      } else{
        warning(
          paste(
            "Requested function",
            current_func_name,
            "is not present in variables please verify the function name is correct"
          ),
          call. = FALSE
        )
      }
    }
    #Create new functions in case of additional params being there
    function_list <- create_exact_function(function_list)
    
    return(function_list)
  }

#Uses the function objects to create a recipy
create_formula_recipy <-
  function(function_object_list,
           working_data,
           variables) {
    # Check variable roles for the recipy creation
    # TODO bllflow support make this add to overall recipy
    outcome_variable <-
      as.character(variables[grepl( "outcome", variables[[pkg.globals$argument.Role]]), pkg.globals$argument.Variables])
    # TODO add a function for creating a proper formula
    recipe_formula <- paste(outcome_variable, "~ .")
    
    recipy_object <-
      recipes::recipe(formula = recipe_formula,
                      data = working_data,
                      x = working_data)
    
    for (single_function in names(function_object_list)) {
      step_formula <- create_formula_variable_formula(function_object_list[[single_function]])
      step_name <- paste("step_", single_function, sep = "")
      recipy_object <-
        do.call(get(step_name), list(recipe = recipy_object,  rlang::parse_expr(unlist(step_formula))))
    }
    recipy_object <-
      recipes::prep(recipy_object, training = working_data)
    
    return(recipy_object)
  }

#Function for creating formula for variable selection
create_formula_variable_formula <- function(functions) {
  return_formula <- list()
  for (variable in names(functions[[pkg.globals$FunctionList.VariableArguments]])) {
    return_formula <- append(return_formula, paste(variable, functions[[pkg.globals$FunctionList.Arguments]], ",",sep = ""))
  }
  return_formula[[length(return_formula)]] <- stringr::str_sub(return_formula[[length(return_formula)]], end = -2)
  return(return_formula)
}