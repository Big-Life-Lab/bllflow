#' Generic center function. Should not be modified
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
center <- function(x, ...) {
  UseMethod("center", x)
}

#' Centers the x arg using the provided mean arg and returns a new bll_flow 
#' variable
#' 
#' @param x numeric,bll_flow The bll_flow variable to center
#' @param mean numeric The mean to center the x arg on
#'
#' @return numeric,bll_flow The centered object
#' @export
#'
#' @examples
center.bll_flow <- function(x, mean) {
  # Create the centered object
  centered_x <- x - mean;
  
  # Make it an instance of the bll_flow class
  bll_flow(centered_x);
  # Add an attribute indicating this is centered
  attr(centered_x, 'center') <- TRUE;
  
  # Return the centered variable
  invisible(centered_x)
}