#' Parse formula step
#'
#' Parse a formula based step
#'
#' @param data data to process
#' @param sequence_number the current step sequence
#' @param modules data.frame containing the module instructions
#' @param variables data.frame containing the variable information
#' @param variable_details data.frame containing variable details
#'
#' @return processed data
parse_formula_step <- function(data,
                               sequence_number,
                               modules,
                               variables,
                               variable_details) {
  working_data <- data
  module_functions <-
    parse_formula_module_functions(
      module_table = modules,
      module_sequence = sequence_number,
      variables = variables,
      variable_details = variable_details
    )
  working_data <-
    create_formula_recipe(module_functions, working_data, variables)
  
  processed_data <- recipes::bake(working_data, new_data = data)
  
  return(processed_data)
}

#' Parse function formula
#'
#' Parse out the functions inside each module returning a list of the functions
#' in it
#'
#' @param module_table data.frame containing module instructions
#' @param module_sequence current module_sequence
#' @param variables data.frame containing variable information
#' @param variable_details data.frame containing variable details
#'
#' @return list of functions for this module step
parse_formula_module_functions <-
  function(module_table,
           module_sequence,
           variables,
           variable_details) {
    # Check row with module sequence
    raw_func <-
      as.character(module_table[module_table[[
        pkg.globals$Modules.DefaultOrder]] == module_sequence,
        pkg.globals$WorkingData.ModuleOperations])
    
    func_list <- strsplit(raw_func, "],")[[1]]
    
    # Seperate each function into seperate list element with 2 elements
    # function name and its arguments
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
        if(grepl("\\}", argument)){
          tmp_arg <- as.list(strsplit(argument, "}")[[1]])
          tmp_arg[[1]] <-
            stringr::str_remove_all(tmp_arg[[1]], "[{}]")
          refactored_funcs_with_args[[func_name]][[
            pkg.globals$FunctionList.Arguments]][[tmp_arg[[1]]]] <-
            tmp_arg[[2]]
        }else{
          tmp_arg <- as.list(strsplit(argument, "=")[[1]])
          tmp_arg[[1]] <- trimws(tmp_arg[[1]])
          refactored_funcs_with_args[[func_name]][[
            pkg.globals$FunctionList.Parameter]][[tmp_arg[[1]]]] <-
            tmp_arg[[2]]
        }
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

#' Parse function variables
#'
#' Uses the function object to find the variables for it and find all
#' the applicable variables
#'
#' @param function_list List containing functions
#' @param variables data.frame containing variable information
#' @param module_sequence_number current module number
#'
#' @return function_list with applicable vars
parse_formula_function_variables <-
  function(function_list,
           variables,
           module_sequence_number) {
    # Check which rows contain the module currently being ran
    affected_rows <-
      variables[grepl(module_sequence_number,
                      variables[[pkg.globals$columnNames.Operations]]), ]
    
    # Check the additional params for the operations and add to function
    for (current_func_name in names(function_list)) {
      if (current_func_name %in% colnames(affected_rows)) {
        columns_to_check <-
          affected_rows[affected_rows[[current_func_name]] != FALSE,
                        c(current_func_name, pkg.globals$columnNames.Variable)]
        
        for (row in 1:nrow(columns_to_check)) {
          function_list[[
            current_func_name]][[
              pkg.globals$FunctionList.VariableArguments]][[
                as.character(columns_to_check[
                  row, pkg.globals$columnNames.Variable])]] <-
            columns_to_check[row, current_func_name]
        }
      } else {
        warning(
          paste(
            "Requested function",
            current_func_name,
            "is not present in variables please verify the function name
            is correct"
          ),
          call. = FALSE
        )
      }
    }
    # Create new functions in case of additional params being there
    function_list <- create_exact_function(function_list)
    
    return(function_list)
  }

#' Uses the function objects to create a recipe
#'
#' @param function_object_list List containing function calls and their
#' arguments
#' @param working_data data.frame that is passed to recipes as training data
#' @param variables data.frame containing specific module steps depending
#' on the variable
#'
#' @importFrom rlang parse_expr
create_formula_recipe <-
  function(function_object_list,
           working_data,
           variables) {
    # Check variable roles for the recipe creation
    # TODO bllflow support make this add to overall recipe
    outcome_variable <-select_vars_by_role("outcome", variables)
    # TODO add a function for creating a proper formula
    recipe_formula <- paste(outcome_variable, "~ .")
    
    recipe_object <-
      recipes::recipe(
        formula = recipe_formula,
        data = working_data,
        x = working_data
      )
    
    for (single_function in names(function_object_list)) {
      step_formula <-
        create_formula_variable_formula(function_object_list[[single_function]])
      step_name <- paste("step_", single_function, sep = "")
      params <- list(recipe = recipe_object,
                     rlang::parse_expr(unlist(step_formula[["vars"]])))
      for (name in names(step_formula[["params"]])) {
        param_to_add <- trimws(step_formula[["params"]][[name]])
        if(!is.na(as.numeric(param_to_add))){param_to_add <- as.numeric(param_to_add)}
        else if(!is.na(as.logical(param_to_add))){param_to_add <- as.logical(param_to_add)}
        params[[name]] <- param_to_add
      }
      recipe_object <-
        do.call(get(step_name),
                params)
    }
    recipe_object <-
      recipes::prep(recipe_object, training = working_data)
    
    return(recipe_object)
  }

#' Variable formula creation
#'
#' Function for creating formula for variable selection
#'
#' @param function list of functions
#'
#' @return list of function call strings
create_formula_variable_formula <- function(functions) {
  return_formula <- list()
  for (variable in names(functions[[
    pkg.globals$FunctionList.VariableArguments]])) {
    return_formula[["vars"]] <- append(return_formula[["vars"]],
                                       paste(variable,
                                             functions[[
                                               pkg.globals$FunctionList.Arguments]],
                                             ",", sep = ""))
  }
  return_formula[["vars"]][[length(return_formula[["vars"]])]] <-
    stringr::str_sub(return_formula[["vars"]][[length(return_formula[["vars"]])]], end = -2)
  
  return_formula[["params"]] <-functions[[
    pkg.globals$FunctionList.Parameter]]
  
  return(return_formula)
}
