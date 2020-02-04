parse_default_step <-
  function(data,
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

#Parse out the functions inside each module returning a list of the functions in it
parse_module_functions <-
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
        # Insert check for formula presense
        tmp_arg <- as.list(strsplit(argument, "=")[[1]])
        tmp_arg[[2]] <-
          stringr::str_remove_all(tmp_arg[[2]], "[\"\"]")
        refactored_funcs_with_args[[func_name]][[pkg.globals$FunctionList.Arguments]][[tmp_arg[[1]]]] <-
          tmp_arg[[2]]
      }
    }
    
    refactored_funcs_with_args_and_vars <-
      parse_function_variables(
        function_list = refactored_funcs_with_args,
        variables = variables,
        module_sequence_number = module_sequence
      )
    
    return(refactored_funcs_with_args_and_vars)
  }

#Uses the function object to find the variables for it and find all the applicable variables
parse_function_variables <-
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

#Define exact functions depending on variable arguments
create_exact_function <- function(function_list) {
  #Parse out non TRUE parameters
  for (fun_name in names(function_list)) {
    if (!is.null(function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]])) {
      for (var_name in names(function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]])) {
        if (function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]] != TRUE) {
          # create new func and move all the ones with that value there
          if (is.null(function_list[[paste(fun_name, "::", function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]], sep = "")]])) {
            function_list[[paste(fun_name, "::", function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]], sep = "")]][[pkg.globals$FunctionList.Arguments]] <-
              function_list[[fun_name]][[pkg.globals$FunctionList.Arguments]]
            function_list[[paste(fun_name, "::", function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]], sep = "")]][[pkg.globals$FunctionList.VariableArguments]][[var_name]] <-
              function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]]
          } else{
            function_list[[paste(fun_name, "::", function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]], sep = "")]][[pkg.globals$FunctionList.VariableArguments]][[var_name]] <-
              function_list[[fun_name]][[pkg.globals$FunctionList.VariableArguments]][[var_name]]
          }
        }
      }
    }
  }
  
  return(function_list)
  #Create new functions with the new paramev ters
}

#Uses the function objects to create a recipy
create_recipy <-
  function(function_object_list,
           working_data,
           variables) {
    # Check variable roles for the recipy creation
    # TODO bllflow support make this add to overall recipy
    outcome_variable <-
      as.character(variables[variables[[pkg.globals$argument.Role]] == "outcome", pkg.globals$argument.Variables])
    # TODO add a function for creating a proper formula
    recipe_formula <- paste(outcome_variable, "~ .")
    
    recipy_object <-
      recipes::recipe(formula = recipe_formula,
                      data = working_data,
                      x = working_data)
    
    for (single_function in names(function_object_list)) {
      variables <-
        names(function_object_list[[single_function]][[pkg.globals$FunctionList.VariableArguments]])
      arguments <-
        function_object_list[[single_function]][[pkg.globals$FunctionList.Arguments]]
      step_formula <- create_variable_formula(variables)
      step_name <- paste("step_", single_function, sep = "")
      step_list <- list(recipe = recipy_object)
      for (argument_name in names(arguments)) {
        argument_name <- trimws(argument_name)
        step_list[argument_name] <- arguments
      }
      step_list[[length(step_list)+1]] <- unlist(step_formula)
      recipy_object <-
        do.call(get(step_name), step_list)
    }
    recipy_object <-
      recipes::prep(recipy_object, training = working_data)
    
    return(recipy_object)
  }

#Function for creating formula for variable selection
create_variable_formula <- function(var_list) {
  return_formula <- list()
  for (variable in var_list) {
    return_formula <- append(return_formula, variable)
  }
  
  return(return_formula)
}