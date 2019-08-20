#' @export
step_meantest <- function(recipe,
                          ...,
                          role = NA,
                          trained = FALSE,
                          skip = FALSE,
                          id = recipes::rand_id("meantest"),
                          append = FALSE,
                          suffix = "_meantest",
                          means = NULL,
                          trim = 0) {
  terms <- recipes::ellipse_check(...)
  recipes::add_step(
    recipe,
    step_meantest_new(
      terms = terms,
      trained = trained,
      role = role,
      append = append,
      suffix = suffix,
      means = means,
      trim = trim,
      skip = skip,
      id = id
    )
  )
}

step_meantest_new <-
  function(terms,
           role,
           trained,
           append,
           suffix,
           means,
           trim,
           skip,
           id) {
    step(
      subclass = "meantest",
      terms = terms,
      role = role,
      trained = trained,
      append = append,
      suffix = suffix,
      means = means,
      trim = trim,
      skip = skip,
      id = id
    )
  }
#' @export
prep.step_meantest <- function(x, training, info = NULL, ...) {
  y <- recipes:::prep.step_meanimpute(x, training, info, ...)
  
  return(
    step_meantest_new(
      terms = x$terms,
      trained = TRUE,
      role = x$role,
      append = x$append,
      suffix = x$suffix,
      means = y$means,
      trim = x$trim,
      skip = x$skip,
      id = x$id
    )
  )
}

#' @export
bake.step_meantest <- function(object, new_data, ...) {
  require(tibble)
  for (varName in names(object$means)) {
    if (any(is.na(new_data[, varName]))) {
      newVarName <- paste(varName, object$suffix, sep = "")
      new_data[newVarName] <- new_data[varName]
      new_data[is.na(new_data[, newVarName]), newVarName] <-
        object$means[[varName]]
      if (!object$append) {
        new_data[varName] <- NULL
      }
    }
  }
  
  return(as_tibble(new_data))
}

#' step_bllflowMeanimpute <-
#'   function(recipe,
#'            ...,
#'            role = NA,
#'            trained = FALSE,
#'            append = TRUE,
#'            suffix = "MeanImpute",
#'            means = NULL,
#'            trim = 0,
#'            skip = FALSE,
#'            id = rand_id("bllflowMeanimpute")) {
#'     add_step(
#'       recipe,
#'       step_bllflowMeanimpute_new(
#'         terms = ellipse_check(...),
#'         role = role,
#'         trained = trained,
#'         append = append,
#'         suffix = suffix,
#'         means = means,
#'         trim = trim,
#'         skip = skip,
#'         id = id
#'       )
#'     )
#'   }
#'
#' step_bllflowMeanimpute_new <-
#'   function(terms, role, trained, means, trim, skip, id) {
#'     step(
#'       subclass = "bllflowMeanimpute",
#'       terms = terms,
#'       role = role,
#'       trained = trained,
#'       append = append,
#'       suffix = suffix,
#'       means = means,
#'       trim = trim,
#'       skip = skip,
#'       id = id
#'     )
#'   }
#'
#' #' @export
#' prep.step_bllflowMeanimpute <- function(x, training, info = NULL, ...) {
#'   col_names <- terms_select(x$terms, info = info)
#'   check_type(training[, col_names])
#'
#'   means <-
#'     vapply(training[, col_names],
#'            mean,
#'            c(mean = 0),
#'            trim = x$trim,
#'            na.rm = TRUE)
#'   step_bllflowMeanimpute_new(
#'     terms = x$terms,
#'     role = x$role,
#'     trained = TRUE,
#'     means,
#'     trim = x$trim,
#'     skip = x$skip,
#'     id = x$id
#'   )
#' }
#'

#'
#' print.step_bllflowMeanimpute <-
#'   function(x, width = max(20, options()$width - 30), ...) {
#'     cat("Mean Imputation for ", sep = "")
#'     printer(names(x$means), x$terms, x$trained, width = width)
#'     invisible(x)
#'   }
#'
#' #' @export
#' tidy.step_bllflowMeanimpute <- function(x, ...) {
#'   if (is_trained(x)) {
#'     res <- tibble(terms = names(x$means),
#'                   model = x$means)
#'   } else {
#'     term_names <- sel2char(x$terms)
#'     res <- tibble(terms = term_names, model = na_dbl)
#'   }
#'   res$id <- x$id
#'   res
#' }