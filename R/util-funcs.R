#' @title is equal
#' @description Function to compare even with NA present
#' This function returns TRUE wherever elements are the same, including NA's,
#' and false everywhere else.
#'
#' @param v1 variable 1
#' @param v2 variable 2
#'
#' @return boolean value of whether or not v1 and v2 are equal
#'
#' @examples
#' library(cchsflow)
#' is_equal(1,2)
#' # FALSE
#'
#' is_equal(1,1)
#' # TRUE
#'
#' 1==NA
#' # NA
#'
#' is_equal(1,NA)
#' # FALSE
#'
#' NA==NA
#' # NA
#'
#' is_equal(NA,NA)
#' # TRUE
#' @export
is_equal <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instances of NA with FALSE
  same[is.na(same)] <- FALSE
  
  return(same)
}


# Cleans strata values
clean_strata_values <-
  function(dim_names) {
    strata_all_combinations_data_frame <- expand.grid(dim_names)
    strata_args <- c(strata_all_combinations_data_frame, sep = ":")
    strata_values <- do.call(paste, strata_args)

    return(strata_values)
  }

#' Vars selected by role
select_vars_by_role <- function(roles, variables){
  # Reduce row looping by only looping over only unique combinations
  unique_roles <- unique(variables[[pkg.globals$argument.Role]])
  valid_patern <- c()
  for (role_patern in unique_roles) {
    # Split by commas to avoid partial matches being false positives
    role_list <- strsplit(role_patern, ",")[[1]]
    for (role in role_list){
      if(role %in% roles){
        valid_patern <- append(valid_patern, role_patern)
      }
    }
  }
  ret <- NULL
  tmp_ret <- variables[variables[[pkg.globals$argument.Role]] == valid_patern, pkg.globals$MSW.Variables.Columns.Variable]
  if(is.data.frame(tmp_ret)){
    ret <- as.character(tmp_ret[[pkg.globals$MSW.Variables.Columns.Variable]])
  }else{
    ret <- tmp_ret
  }
  
  return(ret)
}

# ID role creation
create_id_row <- function(data, id_role_name, database_name, variables){
  # Check for role or variables
  id_cols <- c()
  if(!is.null(id_role_name$feeder_vars)){
    id_cols <- append(id_cols,id_role_name$feeder_vars)
  }else if (!is.null(id_role_name$feeder_roles)){
    id_cols <- append(id_cols, select_vars_by_role(roles = id_role_name$feeder_roles, variables = variables))
  }else {
    message("id_role_name does not contain feeder_roles or feeder_vars.
                  No id column was created")
  }
  if("data_name" %in% id_role_name$feeder_vars && is.null(data[["data_name"]])){
    data[["data_name"]] <- database_name
  }
  tmp_data <- tidyr::unite(data = data, tmp, sep = "_", id_cols)
  data[[id_role_name$var_name]] <- tmp_data$tmp
  
  return(data)
}

# Merge data based on ID
merge_data_on_ID <- function(data_original, data_new, variables, overwrite_rows = TRUE){
  # Identify ID column 
  id_vars <- select_vars_by_role("id", variables)
  
  # Identify similar VARS 
  new_vars <- colnames(data_new)
  original_vars <- colnames(data_original)
  
  identical_vars <- new_vars %in% original_vars
  combined_data <- NULL
  
  # @RUSTY this is risky solution consider other approaches
  data_original[new_vars[!new_vars[identical_vars] %in% id_vars]] <- NULL
  if(overwrite_rows){
    combined_data <- inner_join( data_original, data_new, by= id_vars)
  }else{
    combined_data <- left_join( data_original, data_new, by= id_vars)
  }
   
  return(combined_data)
  # Store duplicates from data_new 
}

