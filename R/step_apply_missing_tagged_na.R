#' Apply missing tagged na
#'
#' `step_apply_missing_tagged_na` creates a *specification* of a recipe step
#'   that will tag variables if they contain NA
#'   or NaN values. This step is based on \code{recipes::step_naomit}
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
#' @param tag_type the type of \code{haven} tag to apply to any na found must
#' be any lowercase letter from a:z
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @rdname step_naomit
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#'
#' @importFrom recipes step
#' @export
step_apply_missing_tagged_na <- function(recipe,
                                         ...,
                                         role = NA,
                                         trained = FALSE,
                                         columns = NULL,
                                         tag_type = NULL,
                                         skip = FALSE,
                                         id =
                                           recipes::rand_id(
                                             "apply_missing_tagged_na")) {
  recipes::add_step(
    recipe,
    step_apply_missing_tagged_na_new(
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

step_apply_missing_tagged_na_new <-
  function(terms, role, trained, columns, tag_type, skip, id) {
    step(
      subclass = "apply_missing_tagged_na",
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
prep.step_apply_missing_tagged_na <- function(x, training, info = NULL, ...) {
  step_apply_missing_tagged_na_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = recipes::terms_select(x$terms, info = info),
    tag_type = x$tag_type,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tibble as.tibble
#' @export
bake.step_apply_missing_tagged_na <- function(object, new_data, ...) {
  for (variable in object$columns) {
    NA_index <- is.na(new_data[[variable]])
    tagged_NA_index <- haven::is_tagged_na(new_data[[variable]])
    true_NA_index <- !(NA_index == tagged_NA_index)
    if (is.numeric(new_data[[variable]])) {
      new_data[true_NA_index, variable] <-
        haven::tagged_na(object$tag_type)
    } else {
      if (!paste("NA(", object$tag_type, ")",
                 sep = "") %in% levels(new_data[[variable]])) {
        levels(new_data[[variable]]) <-
          c(levels(new_data[[variable]]),
            paste("NA(", object$tag_type, ")", sep = ""))
      }
      new_data[true_NA_index, variable] <-
        paste("NA(", object$tag_type, ")", sep = "")
    }
  }
  tibble::as.tibble(new_data)
}
