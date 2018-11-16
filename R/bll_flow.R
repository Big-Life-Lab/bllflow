#' Makes an object an instance of bll_flow and returns it. This function should
#' be run on all the variables in the initial data set
#'
#' @param x numeric The object to convert to bllFlow
#' @param label string Optional label that will be attached to the object
#'
#' @return Returns a new bll_flow object
#' @export
#'
#' @examples
bll_flow <- function(x, label="Not Defined") {
  class(x) <- c('bll_flow', class(x))
  attr(x, 'label') <- label
  
  invisible(x)
}