#verify module sequence matches the passed data
verify_data_and_sequence_match <-
  function(module_sequence_number, data) {
    if (module_sequence_number[[1]] == 1 &&
        attr(data, pkg.globals$bllFlowContent.Sequence) != 0) {
      stop(
        "Working data was passed when sequance is at step 1. Make sure to pass the starting data.
        Aborting operation!"
      )
    } else if (attr(data, pkg.globals$bllFlowContent.Sequence) == 0) {
      if (module_sequence_number[[1]] != 1) {
        stop(
          paste(
            "Non working_data was passed to sequence greater then step 1 please make sure ur passing working data that is result of the module sequence before",
            module_sequence_number,
            "
            Aborting operation!"
          )
          )
      }
      } else if (attr(data, pkg.globals$bllFlowContent.Sequence) + 1 != module_sequence_number[[1]]) {
        stop(
          paste(
            "The WorkingData passed is not from the previous module please verify that the data passed is from module",
            module_sequence_number - 1,
            "
            Aborting operation!"
          )
          )
    }
      }

#' @export
run_module <- function(x, ...) {
  UseMethod("run_module", x)
}

#' @export
run_module.BLLFlow <- function(bll_model, module_sequence_number) {
  #pkg.globals$bllFlowContent.PreviousData
  
  processed_data <-
    run_module.default(
      variables = bll_model$variables,
      modules = bll_model$modules,
      data = bll_model$working_data,
      module_sequence_number = module_sequence_number,
      variable_details = bll_model$variable_details
    )
  bll_model[[pkg.globals$bllFlowContent.PreviousData]] <- processed_data[[2]]
  bll_model[[pkg.globals$bllFlowContent.WorkingData]] <- processed_data[[1]]
  return(bll_model)
}

#' @export
run_module.default <-
  function(variables,
           modules,
           data,
           module_sequence_number,
           variable_details = NULL) {
    #Standardize module_sequence_number
    if (module_sequence_number[[1]] == "all") {
      module_order <- modules[, pkg.globals$Modules.DefaultOrder]
      # Create module_sequence_number out of all default modules
      module_sequence_number <-
        min(module_order, na.rm = TRUE):max(module_order, na.rm = TRUE)
    } else if (!is.numeric(module_sequence_number)) {
      stop(
        "Invalid module_sequence_numberPassed please make sure its either the word all or numeric.
        Aborting operation!",
        call. = FALSE
      )
    }
    
    verify_data_and_sequence_match(module_sequence_number, data)
    
    processed_data <- data
    previous_data <- NULL
    # Find type of module and execute the right call
    for (sequence_element in module_sequence_number) {
      previous_data <- processed_data
      type_of_module <-
        modules[modules[[pkg.globals$Modules.DefaultOrder]] == sequence_element, pkg.globals$Modules.OperationsType]
      if (type_of_module == pkg.globals$ModuleTypes.DefaultStep) {
        processed_data <- parse_default_step(processed_data,
                                             sequence_element,
                                             modules,
                                             variables,
                                             variable_details)
      } else if (type_of_module == pkg.globals$ModuleTypes.FormulaStep) {
        processed_data <- parse_formula_step(processed_data,
                                             sequence_element,
                                             modules,
                                             variables,
                                             variable_details)
      } else if (type_of_module == pkg.globals$ModuleTypes.Function) {
        processed_data <- parse_function(processed_data,
                                         sequence_element,
                                         modules,
                                         variables,
                                         variable_details)
      }
      attr(processed_data, pkg.globals$bllFlowContent.Sequence) <- sequence_element
    }
    
    # Find type of module and execute the right call
    
    return(list(processed_data,previous_data))
  }
