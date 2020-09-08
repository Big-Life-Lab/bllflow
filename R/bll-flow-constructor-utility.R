#' Initialize bllflow object from provided config file
#'
#' Uses the provided config file and matching name to load the correct config
#' type
#'
#' @param config_env_name = NULL name of the config environment to use for
#' initialization
#'
#' @return constructed bllflow object
#' @export
bllflow_config_init <-
  function(config_env_name = NULL) {
    if (!is.null(config_env_name)) {
      set_config_env_name(config_env_name)
      set_config_env_name(config_env_name)
    }
    config <- config::get()
    ret_bllflow <-
      build_bllflow(
        variables = as.data.frame(config$variables),
        variable_details = as.data.frame(config$variable_details),
        modules = as.data.frame(config$modules)
      )

    return(ret_bllflow)
  }

#' Read in data according to config specified data type
#'
#' Uses bllflow object to read_csv_data based on config specifications. 
#' Currently supported formats are: .RData, .csv
#'
#' @param bllflow_object passed bllflow object to read variables from
#' @param config_env_name = NULL optional passing of config if you wish to load data
#' from a specific config
#'
#' @return NULL since no modifications are made and read data is just stored in
#' pre specified location that is read from the config
#' @export
bllflow_config_read_data <- function(bllflow_object, config_env_name = NULL) {
  if (!is.null(config_env_name)) {
    set_config_env_name(config_env_name)
  }
  config <- config::get()
  if (config$data_type == ".RData") {
    # use variables to only read the specified variables??
    for (data_name in names(config$data)) {
      load(config$data[[data_name]])
      save(list = data_name, file = file.path(config$data_dir,
                                              paste0(data_name, ".RData")))
    }
  } else if (config$data_type == ".csv") {
    for (data_name in names(config$data)) {
      tmp_data <-
        read_csv_data(
          variables = bllflow_object$variables,
          data_name = data_name,
          path_to_data = config$data[[data_name]]
        )
      assign(data_name, tmp_data)
      save(list = data_name, file = file.path(config$data_dir,
                                              paste0(data_name, ".RData")))
    }
  }
  return(bllflow_object)
}

#' Recode data using config data
#'
#' Recodes data according to the config then saves it as RData file at a
#' specified location
#'
#' @param bllflow_object passed bllflow object to read variables from
#' @param config_env_name = NULL optional passing of config if you wish to load data
#' from a specific config
#'
#' @return NULL since no modifications are made and read data is just stored in
#' pre specified location
#' @export
bllflow_config_rec_data <- function(bllflow_object, config_env_name = NULL) {
  # Consider making this into a function or let user pass loaded config
  if (!is.null(config_env_name)) {
    set_config_env_name(config_env_name)
  }
  config <- config::get()
  for (data_name in names(config$data)) {
    load(file.path(config$data_dir, paste0(data_name, ".RData")))
    tmp_rec_data <- rec_with_table(
      base::get(data_name),
      variables = bllflow_object$variables,
      variable_details = bllflow_object$variable_details,
      database_name = data_name,
      attach_data_name = TRUE, id_role_name = list(var_name = "row_ID", feeder_vars="ADM_RNO"))
    assign(data_name, tmp_rec_data)
    save(list = data_name,
         file = file.path(config$data_dir,
                          paste0(data_name,
                                 "_recoded",
                                 ".RData")))
  }
  return(bllflow_object)
}

#' Combine data based on config specified location
#'
#' Combines recoded data and applies labels before attaching
#' the data to bllflow object
#'
#' @param bllflow_object passed bllflow object to read variables from
#' @param config_env_name = NULL optional passing of config if you wish to load data
#' from a specific config
#'
#' @return modified bllflow object containing labeled combined data
#' @export
bllflow_config_combine_data <- function(bllflow_object, config_env_name = NULL) {
  if (!is.null(config_env_name)) {
    set_config_env_name(config_env_name)
  }
  config <- config::get()
  tmp_working_data <- NULL
  for (data_name in names(config$data)) {
    load(file.path(config$data_dir, paste0(data_name, "_recoded", ".RData")))
    tmp_mod_data <- base::get(data_name)
    tmp_mod_data[["data_name"]] <- data_name
    # Create unique row id 
    if(config$unique_id){
      id_cols <- select_vars_by_role("ID", bllflow_object$variables)
      
      print("potato")
    }
    if (is.null(tmp_working_data)) {
      tmp_working_data <- tmp_mod_data
    } else {
      tmp_working_data <- dplyr::bind_rows(tmp_working_data, tmp_mod_data)
    }
  }
  tmp_working_data <- bllflow::set_data_labels(
    tmp_working_data,
    bllflow_object$variable_details,
    bllflow_object$variables)
  bllflow_object[[pkg.globals$bllFlowContent.WorkingData]] <- tmp_working_data
  bllflow_object[[pkg.globals$bllFlowContent.PreviousData]] <- tmp_working_data
  attr(bllflow_object[[pkg.globals$bllFlowContent.WorkingData]],
       pkg.globals$bllFlowContent.Sequence) <-
    0

  return(bllflow_object)
}
