#' @export
step_apply_missing_tagged_na <- function(recipe,
                               ...,
                               role = NA,
                               trained = FALSE,
                               columns = NULL,
                               tag_type = NULL,
                               skip = FALSE,
                               id = recipes::rand_id("apply_missing_tagged_na")) {
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

#' @export
bake.step_apply_missing_tagged_na <- function(object, new_data, ...) {
  for (variable in object$columns) {
    NA_index <- is.na(new_data[[variable]])
    tagged_NA_index <- haven::is_tagged_na(new_data[[variable]])
    true_NA_index <- !(NA_index == tagged_NA_index)
    if (is.numeric(new_data[[variable]])) {
      new_data[true_NA_index, variable] <-
        haven::tagged_na(object$tag_type)
    }else{
      if (!paste("NA(", object$tag_type, ")", sep = "") %in% levels(new_data[[variable]])) {
        levels(new_data[[variable]]) <- c(levels(new_data[[variable]]), paste("NA(", object$tag_type, ")", sep = ""))
      }
      new_data[true_NA_index, variable] <- paste("NA(", object$tag_type, ")", sep = "")
    }
  }
  tibble::as.tibble(new_data)
}
