# ---------- DEPRECATED SEE BLLFLOWRECIPES FOR NEW VERSION ----------
#' #' Apply missing tagged NA
#' #'
#' #' Any non tagged NA has the passed tag applied to them this helps tag NA
#' #' that were added by other packages
#' #'
#' #' @param data
#' #' @param variables
#' #' @param tag_type
#' #'
#' #' @return passed data with non tagged NA now having the tag_type applied
#' #' @export
#' apply_missing_tagged_na <- function(data, variables, tag_type) {
#'   for (variable in variables) {
#'     NA_index <- is.na(data[[variable]])
#'     tagged_NA_index <- haven::is_tagged_na(data[[variable]])
#'     true_NA_index <- !(NA_index == tagged_NA_index)
#'     data[true_NA_index, variable] <- haven::tagged_na(tag_type)
#'   }
#'   return(data)
#' }
