#' Verify data sequence match
#' 
#' This functions verifies that the data is from the previous step in the module sequence
#' 
#' @param module_sequence_number the sequence number being checked
#' @param data the data to check
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
#' Run Module
#'
#' Runs modules on x using the module_sequence_numbers. When not using bllflow additional params must be passed.
#' Modules must be ran in correct order ex: module 3 can not be ran until 1 and 2 were ran in that order
#'
#' @param x bllflow object or variables that contain variables alongside variable specific module functions
#' @param ... Used for generic function consistency
#'
#' @return when bllflow object is used a bllflow object is returned with latest module data stored in working data and previous module data stored in previous_module_data
#' @export
run_module <- function(x, ...) {
  UseMethod("run_module", x)
}

#' Bllflow varient of run module
#'
#' Uses bllflow object instead of passing many arguments all of them are retrieved from the bllflow object
#'
#' @param x bllflow object containing module instruction and data to run it on
#' @param module_sequence_number a number specifying the module to run or a numeric range ex: 1 OR 1:3
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
  processed_data[[1]] <- set_data_labels(processed_data[[1]], bll_model$variable_details, bll_model$variables)
  bll_model[[pkg.globals$bllFlowContent.PreviousData]] <- processed_data[[2]]
  bll_model[[pkg.globals$bllFlowContent.WorkingData]] <- processed_data[[1]]
  return(bll_model)
}
#' Non bllflow run module option
#'
#' This is mainly an internal function however for those that dont wish to use bllflow object this is an alternative
#'
#' @param x variables data.frame containing variable specific module instructions
#' @param modules data.frame containing module instructions
#' @param data data.frame to apply the module transformations onto
#' @param module_sequence_number a number specifying the module to run or a numeric range ex: 1 OR 1:3
#' @param variable_details = NULL optional param can be used to attach variable category labels
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
             variable_details = NULL, ...) {
    variables <- x
    # Standardize module_sequence_number
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
        processed_data <- parse_default_step(
          processed_data,
          sequence_element,
          modules,
          variables,
          variable_details
        )
      } else if (type_of_module == pkg.globals$ModuleTypes.FormulaStep) {
        processed_data <- parse_formula_step(
          processed_data,
          sequence_element,
          modules,
          variables,
          variable_details
        )
      } else if (type_of_module == pkg.globals$ModuleTypes.Function) {
        processed_data <- parse_function(
          processed_data,
          sequence_element,
          modules,
          variables,
          variable_details
        )
      }
      attr(processed_data, pkg.globals$bllFlowContent.Sequence) <- sequence_element
    }

    # Find type of module and execute the right call

    return(list(processed_data, previous_data))
  }
