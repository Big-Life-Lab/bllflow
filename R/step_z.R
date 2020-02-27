#' @export
step_z <- function(recipe,
                          ...,
                          role = 'predictor',
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
      stop(paste(variable_name, 'is missing from the training data'))
    }
    if (!is.numeric(training[[variable_name]])) {
      stop(paste(variable_name, 'is not numeric therefore zScore cannot 
                 be calculated'))
    }
    # Calculate the Standard Deviation for the variable
    x$means[[variable_name]] <- mean(training[[variable_name]], trim = x$trim, na.rm = x$na.rm)
    x$sd[[variable_name]] <- sd(training[[variable_name]], na.rm = x$na.rm)
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
  require(tibble)
  for (varName in names(object$means)) {
    newVarName <- paste(varName, object$suffix, sep = "")
    new_data[newVarName] <- (new_data[[varName]] - object$means[[varName]]) / object$sd[[varName]]
    if (!object$append) {
      new_data[varName] <- NULL
    }
    
  }
  
  return(as_tibble(new_data))
}

print.step_z <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("z score for ", sep = "")
    recipes:::printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }
