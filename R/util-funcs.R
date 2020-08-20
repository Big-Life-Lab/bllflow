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

