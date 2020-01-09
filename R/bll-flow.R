#' Creates a bllflow model
#'
#' Wraps up the data, variables and variable_details arguments in an R object,
#' making it an instance of a bllflow class and returning the resulting object.
#' If a ddi argument is provided, all the metadata from the DDI document is
#' imported into the R object
#'
#' @param data_list A dataframe list that represents the datasets the model will be developed
#' on
#' @param variables A dataframe that has the specification sheet for this model. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=0}.
#' @param variable_details A dataframe that is the variable details worksheet. An example
#' of this worksheet is available here
#' \url{https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1196358036}.
#' @param ddi_list A named list that contains the ddi documents
#' @return A named list which is an instance of the bllflow class. The items
#' in the list are specified below: \cr
#' 1. data_list - A dataframe that contains the passed data_list argument \cr
#' 2. variables - A dataframe that contains the passed variables argument \cr
#' 3. variable_details - A dataframe that contains the passed variable_details argument \cr
#' 4. ddi_list - A named list that contains the ddi found on the passed path \cr
#' 5. additional_DDI_meta_data - A named list. See the return type of the \code{\link{get_DDI_description}} function \cr
#' 6. populated_variable_details - A dataframe that contains the rows in the variable_details \cr
#' argument but with additional data filled in using the ddi argument it's specified
#'
#' @export
#'
#' @examples
#' #TODO Update with lists
#' # ALl the libraries we will be using
#' library(bllflow)
#' library(survival)
#'
#' # Read in the data we will use for this example
#' data(pbc)
#'
#' # Read in the variables and variable details CSV sheets which are part of the
#' # master specification workbook
#' variablesSheet <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' variable_details <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' # and store it in the pbcModel variable
#' pbcModel <- bllflow::BLLFlow(pbc, variablesSheet, variable_details)
#'
#' # The pbcModel variable is an R object of instance BLLFlow
#' print(attr(pbcModel, 'class'))
BLLFlow <-
  function(data_list = NULL,
           variables = NULL,
           variable_details = NULL,
           ddi_list = NULL) {
    ddi_header <- list()
    # Verify passed arg integrity for future functions
    if (!is.null(data_list)) {
      for (single_data_index in 1:length(data_list)) {
        check_if_data_frame(data_list[[single_data_index]], names(data_list)[[single_data_index]])
      }
      
    }
    if (!is.null(variables)) {
      check_if_data_frame(variables, pkg.globals$argument.Variables)
      # Change the columns needed for the functions
      check_for_column_presence(
        c("variable", "label", "labelLong", "variableType", "units"),
        variables,
        pkg.globals$argument.Variables
      )
    }
    if (!is.null(variable_details)) {
      check_if_data_frame(variable_details,
                       pkg.globals$argument.VariableDetailsSheet)
      check_for_column_presence(
        c(
          "variable",
          "toType",
          "databaseStart",
          "variableStart",
          "fromType",
          "recTo",
          "catLabel",
          "catLabelLong",
          "recFrom",
          "units"
        ),
        variable_details,
        pkg.globals$argument.VariableDetailsSheet
      )
    }
    
    
    if (!is.null(ddi_list)) {
      # TODO redisign to create template rather then populate add a check to verify proper structure
      # processedVariableDetails <-
      #   ProcessDDIVariableDetails(ddi, variable_details)
      for (ddi_index in 1:length(ddi_list)) {
        check_for_existance_of_in_list(c("variableMetaData", "ddiObject"),
                                  ddi_list[[ddi_index]],
                                  paste(names(ddi_list)[[ddi_index]], "ddi"))
        ddi_header[[names(ddi_list)[[ddi_index]]]] <-
          get_DDI_description(ddi_list[[ddi_index]])
      }
      
    } else{
      ddi_header <- NULL
    }
    bll_flow_model <-
      list(
        data_list = data_list,
        variables = variables,
        variable_details = variable_details,
        additional_DDI_meta_data = ddi_header,
        populated_variable_details = NULL,
        ddi_list = ddi_list
        
      )
    attr(bll_flow_model, "class") <- "BLLFlow"
    
    return(bll_flow_model)
  }

#' @export
read_data <- function(variables, data_name, path_to_data, nrows = -1) {
  # calculate the rows to set to null
  first_row_of_data <- read.csv(file = path_to_data, nrows = 1)
  
  var_names_for_this_data <- get_variables.default(variables, data_name)
  
  columns_to_keep <- colnames(first_row_of_data) %in% var_names_for_this_data
  column_classes <- sapply(columns_to_keep, boolean_conversion)
  
  data_to_save <- read.csv(file = path_to_data,
           colClasses = column_classes, nrows = nrows)
  
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
#' @export
get_variables <- function(variable_source = NULL, ...){
  UseMethod("get_variables", variable_source)
}
#' @export
get_variables.BLLFlow <- function(bllFlow, data_name){
  variables <- bllFlow[[pkg.globals$bllFlowContent.Variables]]
  
  return(get_variables.default(variables, data_name))
}
#' @export
get_variables.default <- function(variables, data_name){
  variables_to_read_list <-
    variables[grepl(data_name, variables[[pkg.globals$argument.DatabaseStart]]), ]

  var_names_for_this_data <- list()
  
  for (variable_to_read_row in 1:nrow(variables_to_read_list)) {
    variable_to_read <-
      as.character(variables_to_read_list[variable_to_read_row, pkg.globals$argument.VariableStart])
    data_variable_being_checked <- character()
    if (!grepl("DerivedVar", variable_to_read)) {
      if (grepl(data_name, variable_to_read)) {
        var_start_names_list <- as.list(strsplit(variable_to_read, ",")[[1]])
        # Find exact var Name
        for (var_name in var_start_names_list) {
          if (grepl(data_name, var_name)) {
            # seperate dataname from the var name
            data_variable_being_checked <-
              as.list(strsplit(var_name, "::")[[1]])[[2]]
          }
        }
      } else if (grepl("\\[", variable_to_read)) {
        data_variable_being_checked <-
          stringr::str_match(variable_to_read, "\\[(.*?)\\]")[, 2]
      }
    }
    
    var_names_for_this_data <-
      append(var_names_for_this_data, data_variable_being_checked)
  }
  var_names_for_this_data <- unique(var_names_for_this_data)
  
  return(var_names_for_this_data)
}
