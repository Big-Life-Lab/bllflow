#' Remove observations with missing values
#'
#' `step_tagged_naomit` creates a *specification* of a recipe step that
#'   will add remove observations (rows of data) if they contain NA
#'   or NaN values.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the dummy variables. See
#'  [selections()] for more details. The selected
#'  variables must be factors.
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#'
#' @rdname step_tagged_naomit
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#'
#' recipe(Ozone ~ ., data = airquality) %>%
#'   step_tagged_naomit(Solar.R) %>%
#'   prep(airquality, verbose = FALSE) %>%
#'   juice()
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_tagged_naomit <- function(recipe,
                        ...,
                        role = NA,
                        trained = FALSE,
                        columns = NULL,
                        tag_type = NULL,
                        skip = FALSE,
                        id = recipes::rand_id("tagged_naomit")) {
  recipes::add_step(
    recipe,
    step_tagged_naomit_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      tag_type = trimws(tag_type),
      skip = skip,
      id = id
    )
  )
}

step_tagged_naomit_new <-
  function(terms, role, trained, columns, tag_type, skip, id) {
    step(
      subclass = "tagged_naomit",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      tag_type = tag_type,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tagged_naomit <- function(x, training, info = NULL, ...) {
  step_tagged_naomit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = recipes::terms_select(x$terms, info = info),
    tag_type = x$tag_type,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tagged_naomit <- function(object, new_data, ...) {
  if (is.null(object$tag_type)) {
    tibble::as_tibble(tidyr::drop_na(new_data, object$columns))
  } else{
    for (column in object$columns) {
      if(is.numeric(new_data[[column]])){
      new_data <- new_data[!haven::is_tagged_na(new_data[[column]], tag = object$tag_type),]
      }else{
        new_data <- new_data[!is_equal(new_data[[column]], paste("NA(",object$tag_type,")",sep = "")), ]
      }
    }
    tibble::as.tibble(new_data)
  }
}

print.step_tagged_naomit <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Removing rows with NA values in ", sep = "")
    cat(tidyr::drop_naformat_selectors(x$terms, width = width))
    cat("\n")
    invisible(x)
  }

#' @rdname step_tagged_naomit
#' @param x A `step_tagged_naomit` object.
#' @export
tidy.step_tagged_naomit <- function(x, ...) {
  res <- recipes::simple_terms(x, ...)
  res$id <- x$id
  res
}
