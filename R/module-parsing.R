#' Run Module
#'
#' Runs modules on x using the module_sequence_numbers. When not using bllflow
#' additional params must be passed.
#' Modules must be ran in correct order ex: module 3 can not be ran until
#' 1 and 2 were ran in that order
#'
#' @param x bllflow object or variables that contain variables alongside
#' variable specific module functions
#' @param ... Used for generic function consistency
#'
#' @return when bllflow object is used a bllflow object is returned with latest
#' module data stored in working data and previous module
#' data stored in previous_module_data
#' @export
run_module <- function(x, ...) {
  UseMethod("run_module", x)
}

#' Bllflow variant of run module
#'
#' Uses bllflow object instead of passing many arguments all of them are
#' retrieved from the bllflow object
#'
#' @param x bllflow object containing module instruction and data to run it on
#' @param module_sequence_number a number specifying the module to run
#' or a numeric range ex: 1 OR 1:3
#' @param ... Used for generic function consistency
#'
#' @export
run_module.BLLFlow <- function(x, module_sequence_number, ...) {
  bll_model <- x
  # pkg.globals$bllFlowContent.PreviousData
  
  processed_data <-
    run_module.default(
      x = bll_model$variables,
      modules = bll_model$modules,
      data = bll_model$working_data,
      module_sequence_number = module_sequence_number,
      variable_details = bll_model$variable_details
    )
  processed_data[[1]] <- as.data.frame(processed_data[[1]])
  processed_data[[1]] <- set_data_labels(processed_data[[1]],
                                         bll_model$variable_details,
                                         bll_model$variables)
  # Append changed data to data @RUSTY
  bll_model[[pkg.globals$bllFlowContent.PreviousData]] <-
    processed_data[[2]]
  bll_model[[pkg.globals$bllFlowContent.WorkingData]] <-
    processed_data[[1]]
  return(bll_model)
}
#' Non bllflow run module option
#'
#' This is mainly an internal function however for those that dont wish to use
#' bllflow object this is an alternative
#'
#' @param x variables data.frame containing variable specific module
#' instructions
#' @param modules data.frame containing module instructions
#' @param data data.frame to apply the module transformations onto
#' @param module_sequence_number a number specifying the module to run or
#' a numeric range ex: 1 OR 1:3 or an allowed string: "all"
#' @param variable_details = NULL optional param can be used
#' to attach variable category labels
#' @param ... Used for generic function consistency
#'
#' @importFrom recipes bake
#' @importFrom recipes prep
#' @importFrom recipes tidy
#' @export
run_module.default <-
  function(x,
           modules,
           data,
           module_sequence_number,
           variable_details = NULL,
           ...) {
    variables <- x
    # Standardize module_sequence_number
    if (module_sequence_number[[1]] == "all") {
      module_order <- modules[, pkg.globals$Modules.DefaultOrder]
      # Create module_sequence_number out of all default modules
      module_sequence_number <-
        min(module_order, na.rm = TRUE):max(module_order, na.rm = TRUE)
    } else if (!is.numeric(module_sequence_number)) {
      stop(
        "Invalid module_sequence_number passed. Please make sure
        its either the word \"all\" or numeric.
        Aborting operation!",
        call. = FALSE
      )
    }
    
    verify_data_and_sequence_match(module_sequence_number, data)
    
    processed_data <- data
    previous_data <- NULL
    # Find type of module and execute the right call
    for (sequence_number in module_sequence_number) {
      # Select data subset based on what is included in the step @RUSTY
      previous_data <- processed_data
      processed_data <-
        module_data_subset(data = processed_data,
                           variables = variables,
                           sequence_number = sequence_number)
      var_rows <- processed_data[[2]]
      processed_data <- processed_data[[1]]
      
      processed_data <-
        parse_module(var_rows, sequence_number, processed_data, modules)
      
      # @RUSTY merge data based on unique id 
      processed_data <- merge_data_on_ID(data, processed_data, variables)
      
      attr(processed_data, pkg.globals$bllFlowContent.Sequence) <-
        sequence_number
    }
    
    return(list(processed_data, previous_data))
  }

module_data_subset <- function(data, variables, sequence_number) {
  # vector containing vars that match sequence_number
  unique_module_ID <-
    as.list(unique(variables[, pkg.globals$columnNames.Operations])) #@RUSTY merge with select_vars_by_role
  valid_ID_patern <- c()
  for (ID_patern in unique_module_ID) {
    # Split by commas to avoid partial matches being false positives
    ID_list <- strsplit(ID_patern, ",")[[1]]
    for (ID in ID_list) {
      if (ID %in% sequence_number) {
        valid_ID_patern <- append(valid_ID_patern, ID_patern)
      }
    }
  }
  id_vars <- select_vars_by_role("id",variables)
  valid_rows_index <- (variables[[pkg.globals$columnNames.Operations]] == valid_ID_patern) | (variables[[pkg.globals$columnNames.Variable]] %in% id_vars)
  valid_rows_index[is.na(valid_rows_index)] <- FALSE
  valid_rows <-
    variables[valid_rows_index,]
  valid_vars <-
    as.character(valid_rows[[pkg.globals$MSW.Variables.Columns.Variable]])
  ret_data <- data[, valid_vars]
  if(!is.data.frame(ret_data)){
    ret_data <- as.data.frame(ret_data)
    colnames(ret_data) <- valid_vars
  }
  
  return(list(ret_data, valid_rows))
}

parse_module <- function(variables, module_ID, data, modules) {
  # Isolate individual operations
  operations_to_run <-
    as.character(modules[modules[[pkg.globals$Modules.ModuleID]] == module_ID, pkg.globals$columnNames.Operations])
  operations_list <- strsplit(operations_to_run, "],")[[1]]
  running_data <- data
  func_list <- c()
  # Step 1: Parse out function requirements
  for (single_function in operations_list) {
    # Seperate package name from function name
    # @RUSTY add a potential check for the package being loaded/ having that function
    single_parsed_func <- list()
    pkg_name <- strsplit(single_function, "::")[[1]]
    single_function <- pkg_name[[2]]
    single_parsed_func$pkg_name <- pkg_name[[1]]
    
    func_name <- strsplit(single_function, "\\[")[[1]]
    single_function <- func_name[[2]]
    single_parsed_func$func_name <- func_name[[1]]
    
    vars_used <- list()
    
    func_args <- strsplit(single_function, ",")[[1]]
    single_parsed_func$args <- c()
    for (single_arg in func_args) {
      # Catch "formula" argument
      if (grepl("role\\(", single_arg)) {
        roles_list <-
          stringr::str_extract_all(single_arg, "role\\(\\s*(.*?)\\s*\\)")
        for (role in roles_list) {
          real_role_name <-
            stringr::str_extract_all(role, "\\([^()]+\\)")[[1]]
          real_role_name <-
            substring(real_role_name, 2, nchar(real_role_name) - 1)
          vars <- select_vars_by_role(real_role_name, variables)
          vars_used[[real_role_name]] <- vars
          single_arg <-
            gsub(paste0("role", "\\(", real_role_name, "\\)"),
                 vars,
                 single_arg)
        }
      }
      if (grepl("formula\\(", single_arg)) {
        single_arg <- gsub("formula\\(|\\)", "", single_arg)
      } else{
        tmp_arg <- as.list(strsplit(single_arg, "=")[[1]])
        tmp_arg[[1]] <- trimws(tmp_arg[[1]])
        tmp_arg[[2]] <- gsub("\\[|\\]", "", tmp_arg[[2]])
        single_parsed_func$params[[tmp_arg[[1]]]] <- tmp_arg[[2]]
        single_arg <- ""
      }
      single_arg <- gsub("\\[|\\]", "", single_arg)
      single_arg <- trimws(single_arg)
      single_parsed_func$args <-
        append(single_parsed_func$args, single_arg)
    }
    single_parsed_func$all_vars <- vars_used
    
    func_list <- append(func_list, list(single_parsed_func))
  }
  
  # Step 2: Create function calls
  recipy_flag <- FALSE
  # Setting scope outside loop
  recipe_object <- NULL
  for (single_func in func_list) {
    # Detect step in the function name
    if (grepl("step_", single_func$func_name)) {
      if (!recipy_flag) {
        recipy_flag <- TRUE
        # Create recipy
        outcome_variable <- single_func$all_vars$outcome
        if (is.null(outcome_variable)) {
          outcome_variable <- "."
        }
        recipe_formula <- paste(outcome_variable, "~ .")
        recipe_object <-
          recipes::recipe(formula = recipe_formula,
                          x = data)
        
        # Remove default predictor role if output was suplied
        if (outcome_variable != "."){
        recipe_object <-
          remove_role(recipe_object, all_predictors(), old_role = "predictor")
        }
        
        # Assign roles
        for (new_role in names(single_func$all_vars)) {
          params <- list(recipe = recipe_object, rlang::parse_expr(unlist(single_func$all_vars[[new_role]])), new_role = new_role)
          do.call(add_role, params)
        }
      }
      # Add to recipe
      params <- list(recipe = recipe_object, rlang::parse_expr(unlist(single_func$args)))
      for (param_name in names(single_func$params)) {
        param_to_add <- trimws(single_func$params[[param_name]])
        if(!is.na(as.numeric(param_to_add))){param_to_add <- as.numeric(param_to_add)}
        else if(!is.na(as.logical(param_to_add))){param_to_add <- as.logical(param_to_add)}
        params[[param_name]] <- param_to_add
      }
      recipe_object <- do.call(get(single_func$func_name), params)
      
    }else{
      # Check for running recipy
      if(recipy_flag){
        # Bake existing recipy and update working data to run non recipy function on
        recipe_object <-
          recipes::prep(recipe_object, training = data)
        data <- recipes::bake(recipe_object, new_data = data)
        # @RUSTY detect multi data output in case of inputes
        # Reference config for RDF inpute storage and store other data before removal make sure to pair with unique id{ function out the check}
        recipy_flag <- FALSE
        params <- list(data = data, rlang::parse_expr(unlist(single_func$args)))
        for (param_name in names(single_func$params)) {
          param_to_add <- trimws(single_func$params[[param_name]])
          if(!is.na(as.numeric(param_to_add))){param_to_add <- as.numeric(param_to_add)}
          else if(!is.na(as.logical(param_to_add))){param_to_add <- as.logical(param_to_add)}
          params[[param_name]] <- param_to_add
        }
        
        tmp_data <- do.call(get(single_func$func_name), params)
        if(!is.data.frame(tmp_data)){
          #Check for dataframe within list if possible if not throw error
        }
      }
      
    }
  }
  
  # Bake if not yet baked
  if(recipy_flag){
    # Bake existing recipy and update working data to run non recipy function on
    recipe_object <-
      recipes::prep(recipe_object, training = data)
    data <- recipes::bake(recipe_object, new_data = data)
  }
  
  return(data)
}

#' Verify data sequence match
#'
#' This functions verifies that the data is from the previous step in
#' the module sequence
#'
#' @param module_sequence_number the sequence number being checked
#' @param data the data to check
verify_data_and_sequence_match <-
  function(module_sequence_number, data) {
    if (module_sequence_number[[1]] == 1 &&
        attr(data, pkg.globals$bllFlowContent.Sequence) != 0) {
      stop(
        "Working data was passed where sequence is at step 1.
        Make sure to pass the starting data.
        Aborting operation!"
      )
    } else if (attr(data, pkg.globals$bllFlowContent.Sequence) == 0) {
      if (module_sequence_number[[1]] != 1) {
        stop(
          paste(
            "Non-working data was passed to sequence greater then step 1.
            Please make sure that you are passing working data that is the result of the
            module sequence before",
            module_sequence_number,
            "
            Aborting operation!"
          )
        )
      }
    } else if (attr(data, pkg.globals$bllFlowContent.Sequence) + 1 !=
               module_sequence_number[[1]]) {
      stop(
        paste(
          "The working data passed is not from the previous module please
          verify that the data passed is from module",
          module_sequence_number - 1,
          "
            Aborting operation!"
        )
      )
    }
  }