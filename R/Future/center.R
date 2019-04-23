#' Centers the x argument using the provided mean argument and returns a new bll_flow
#' variable
#'
#' @param x numeric,bll_flow The bll_flow variable to center
#' @param mean numeric The mean to center the x arg on
#'
#' @return numeric,bll_flow The centered object with attribute (attr) = center and 'label' = 'Centered'
#' @export
#'
#' @examples
center.bll_flow <- function(x, mean) {
  # Create the centered object
  centered_x <- x - mean

  # Make it an instance of the bll_flow class
  bll_flow(centered_x, cat(attr(x, "label"), "Centered"))
  # Add an attribute indicating this is centered
  attr(centered_x, "center") <- TRUE

  # Return the centered variable
  invisible(centered_x)
}
