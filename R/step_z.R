#' Calculate z score
#'
#' `step_z` creates a *specification* of a recipe step that
#'   will calculate z-score on specified variables using training set mean and
#'   standard deviation, then based on append
#'   will overwrite existing variable or add new columns
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @param append A boolean indicator if the calculated z-score is to be appened
#' or replace the original variable
#' @param suffix A character indicating the suffix for the variable
#' @param means A list used for storing the means calculated during prep
#' @param sd A list used for storing the standard deviation during prep
#' @param na.rm na.rm paramater to pass to the mean, sd functions
#' @param trim paramater to pass to the mean and sd functions
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the mean
#'  value).
#'
#'
#' @importFrom stats sd
#' @importFrom recipes step
#' @export
step_z <- function(recipe,
                   ...,
                   role = "predictor",
                   trained = FALSE,
                   skip = FALSE,
                   id = recipes::rand_id("Z"),
                   append = TRUE,
                   suffix = "_Z",
                   means = NULL,
                   sd = NULL,
                   na.rm = TRUE,
                   trim = 0) {
  terms <- recipes::ellipse_check(...)
  recipes::add_step(
    recipe,
    step_z_new(
      terms = terms,
      trained = trained,
      role = role,
      append = as.logical(append),
      suffix = suffix,
      means = means,
      sd = sd,
      na.rm = na.rm,
      trim = trim,
      skip = skip,
      id = id
    )
  )
}

step_z_new <-
  function(terms,
             role,
             trained,
             append,
             suffix,
             means,
             sd,
             na.rm,
             trim,
             skip,
             id) {
    step(
      subclass = "z",
      terms = terms,
      role = role,
      trained = trained,
      append = append,
      suffix = suffix,
      means = means,
      sd = sd,
      na.rm = na.rm,
      trim = trim,
      skip = skip,
      id = id
    )
  }
#' @export
prep.step_z <- function(x, training, info = NULL, ...) {
  for (variable_name in recipes::terms_select(x$terms, info = info)) {
    # Verify the training data variable
    if (is.null(training[[variable_name]])) {
      stop(paste(variable_name, "is missing from the training data"))
    }
    if (!is.numeric(training[[variable_name]])) {
      stop(paste(variable_name, "is not numeric therefore zScore cannot \n                 be calculated"))
    }
    # Calculate the Standard Deviation for the variable
    x$means[[variable_name]] <- mean(training[[variable_name]], trim = x$trim, na.rm = x$na.rm)
    x$sd[[variable_name]] <- stats::sd(training[[variable_name]], na.rm = x$na.rm)
    # Calculate the mean for the variable
  }

  return(
    step_z_new(
      terms = x$terms,
      trained = TRUE,
      role = x$role,
      append = x$append,
      suffix = x$suffix,
      means = x$means,
      sd = x$sd,
      na.rm = x$na.rm,
      trim = x$trim,
      skip = x$skip,
      id = x$id
    )
  )
}

#' @export
bake.step_z <- function(object, new_data, ...) {
  for (varName in names(object$means)) {
    newVarName <- paste(varName, object$suffix, sep = "")
    new_data[newVarName] <- (new_data[[varName]] - object$means[[varName]]) / object$sd[[varName]]
    if (!object$append) {
      new_data[varName] <- NULL
    }
  }

  return(tibble::as_tibble(new_data))
}

print.step_z <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("z score for ", sep = "")
    recipes::printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }
