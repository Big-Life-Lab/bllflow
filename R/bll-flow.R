#' Creates a bllflow model
#'
#' Wraps up the data, variables and variable_details arguments in an R object,
#' making it an instance of a bllflow class and returning the resulting object.
#'
#' @param data A dataframe that represents the dataset the model
#'  will be developed on
#' @param variables A dataframe that has the specification sheet for this model.
#'  An example of this worksheet is available here
#'  \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit?usp=sharing}.
#' @param variable_details A dataframe that is the variable details worksheet.
#'  An example of this worksheet is available here
#'  \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit?usp=sharing}.
#' @param modules A dataframe containing module instructions
#' @return A named list which is an instance of the bllflow class. The items
#'  in the list are specified below: \cr
#'  1. previous_module_data - A dataframe used to store previous module
#'   transformation for the purpose of backtracking \cr
#'  2. working_data - A dataframe used to store latest module transformations.
#'   This is the primary data used in bllflow functions \cr
#'  3. variables - A dataframe that contains the passed variables argument \cr
#'  4. variable_details - A dataframe that contains the passed
#'   variable_details argument \cr
#'
#' @export
build_bllflow <-
  function(data = NULL,
           variables = NULL,
           variable_details = NULL,
           modules = NULL) {
    # Verify passed arg integrity for future functions
    if (!is.null(data)) {
      check_if_data_frame(data, names(data))
    }
    if (!is.null(variables)) {
      check_if_data_frame(variables, pkg.globals$argument.Variables)
      # Change the columns needed for the functions
      check_for_column_presence(
        c(pkg.globals$MSW.Variables.Columns.Variable, pkg.globals$MSW.Variables.Columns.Label, pkg.globals$MSW.Variables.Columns.LabelLong, pkg.globals$MSW.Variables.Columns.VariableType, pkg.globals$MSW.Variables.Columns.Units),
        variables,
        pkg.globals$argument.Variables
      )
    }
    if (!is.null(variable_details)) {
      check_if_data_frame(variable_details,
                          pkg.globals$argument.VariableDetailsSheet)
      check_for_column_presence(
        c(
          pkg.globals$argument.Variables,
          pkg.globals$argument.ToType,
          pkg.globals$argument.DatabaseStart,
          pkg.globals$argument.VariableStart,
          pkg.globals$argument.FromType,
          pkg.globals$argument.recEnd,
          pkg.globals$argument.CatLabel,
          pkg.globals$argument.CatLabelLong,
          pkg.globals$argument.recStart,
          pkg.globals$argument.Units
        ),
        variable_details,
        pkg.globals$argument.VariableDetailsSheet
      )
    }

    bll_flow_model <-
      list(
        previous_module_data = data,
        working_data = data,
        variables = variables,
        variable_details = variable_details,
        modules = modules
      )
    attr(bll_flow_model, "class") <- pkg.globals$bllFlowContent.Class
    
    if (!is.null(bll_flow_model[[pkg.globals$bllFlowContent.WorkingData]])) {
      attr(bll_flow_model[[pkg.globals$bllFlowContent.WorkingData]],
           pkg.globals$bllFlowContent.Sequence) <-
        0
    }

    return(bll_flow_model)
  }
#' Read csv data
#'
#' Uses passed variables sheet as well as the name of the data name to only read
#' the needed columns from the csv
#'
#' @param variables a variables dataframe can usually be found in
#'  bllflow$variables
#' @param data_name the name of the database you're reading make sure it matches
#'  the name in databaseStart
#' @param path_to_data a valid file path to read the csv from
#' @param nrows = -1 specifies the number of rows to read in case not full data
#'  is required
#'
#' @return a data.frame containing only the variables needed for that cycle
#'  in variables
#' @export
read_csv_data <-
  function(variables,
           data_name,
           path_to_data,
           nrows = -1) {
    # calculate the rows to set to null
    first_row_of_data <-
      utils::read.csv(file = path_to_data, nrows = 1)

    var_names_for_this_data <-
      get_variables(variables, data_name)

    columns_to_keep <-
      colnames(first_row_of_data) %in% var_names_for_this_data
    column_classes <- sapply(columns_to_keep, boolean_conversion)

    data_to_save <- utils::read.csv(file = path_to_data,
                                    colClasses = column_classes,
                                    nrows = nrows)

    return(data_to_save)
  }
boolean_conversion <- function(bool_value) {
  ret_value <- character()
  if (bool_value) {
    ret_value <- NA
  } else {
    ret_value <- "NULL"
  }

  return(ret_value)
}
#' Get variables
#'
#' Generic method for getting start variables from variable_source
#' based on data_name
#'
#' @param variable_source A variables sheet or bllflow object containing
#'  the variables sheet
#' @param ... Used for generic function consistency
#'
#' @return character vector containing start variables for this databaseStart
#' @export
get_variables <- function(variable_source = NULL, ...) {
  UseMethod("get_variables", variable_source)
}

#' Get variables from bllflow using specified database name
#'
#' A bllflow specific wrapper around default get_variables to allow
#' consistent bllflow workflow
#'
#' @param variable_source a bllflow object containing variables sheet
#'  that is then parsed
#' @param data_name name matching the databaseStart column you wish to
#'  get variables for
#' @param ... Used for generic function consistency
#'
#' @return character vector containing start variables for this databaseStart
#' @export
get_variables.BLLFlow <- function(variable_source, data_name, ...) {
  variables <- variable_source[[pkg.globals$bllFlowContent.Variables]]

  return(get_variables(variables, data_name))
}

#' Get variables from variables using specified database name
#'
#' @param variable_source a variables data.frame object containing variables
#'  used in the study
#' @param data_name name matching the databaseStart column you wish to get
#'  variables for
#' @param ... Used for generic function consistency
#'
#' @return character vector containing start variables for this databaseStart
#' @export
get_variables.default <- function(variable_source, data_name, ...) {
  variables <- variable_source
  variables_to_read_list <-
    variables[grepl(data_name, 
                    variables[[pkg.globals$argument.DatabaseStart]]), ]

  var_names_for_this_data <- list()

  for (variable_to_read_row in seq_len(nrow(variables_to_read_list))) {
    variable_to_read <-
      as.character(variables_to_read_list[variable_to_read_row,
                                          pkg.globals$argument.VariableStart])
    data_variable_being_checked <- character()
    var_start_names_list <-
      as.list(strsplit(variable_to_read, ",")[[1]])
    # Loop through all the elements of variableStart
    for (var_name in var_start_names_list) {
      # If the data name is contained means the <data>::<var> format is followed
      if (grepl(data_name, var_name)) {
        # separate dataname from the var name
        # Derived vars dont have a single variable start so they are ignored
        if (!grepl("DerivedVar", var_name)) {
          data_variable_being_checked <-
            as.list(strsplit(var_name, "::")[[1]])[[2]]
        }
      }
    }
    # Once all the elements are checked the default var name is then selected [<var>]
    if (length(data_variable_being_checked) == 0) {
      # The default is the last element in the cell
      last_var_list_element <-
        var_start_names_list[[length(var_start_names_list)]]
      # Check that last element is indeed default value
      if (grepl("\\[", last_var_list_element)) {
        # Strip the [] resulting in clean var name
        data_variable_being_checked <-
          stringr::str_match(last_var_list_element, "\\[(.*?)\\]")[, 2]
      }
    }
  
  
    var_names_for_this_data <-
      append(var_names_for_this_data, data_variable_being_checked)
  }
  var_names_for_this_data <- unique(var_names_for_this_data)

  return(var_names_for_this_data)
}
