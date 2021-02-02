
#' @export
create_bllflow_recipe <-
  function(bllflow_object, module_name, roles) {
    recipe_vars <- select_vars_by_role(roles, bllflow_object$variables)
    existing_roles <-
     as.character(unique(bllflow_object$variables[bllflow_object$variables[[pkg.globals$columnNames.Variable]] %in%
                                        recipe_vars , pkg.globals$argument.Role])[[pkg.globals$argument.Role]])
    single_roles <- c()
    for (single_set in existing_roles) {
      single_set <- strsplit(single_set, split = ",")[[1]]
      single_roles <- c(single_roles,single_set[!single_set %in% single_roles])
    }
    
    
    data <- bllflow_object$working_data
    # Temporary patch for data missmatch
    recipe_vars <- recipe_vars[recipe_vars %in% colnames(data)]
    data <- data[, recipe_vars]

    # Add formula creation
    recipe_formula <- paste(".", "~ .")
    recipe_object <-
      recipes::recipe(formula = recipe_formula,
                      x = data)
    all_vars <- list()
    for (single_role in single_roles) {
      all_vars[[single_role]] <- select_vars_by_role(single_role, bllflow_object$variables)
    }
    
    
    for (new_role in names(all_vars)) {
      params <-
        list(recipe = recipe_object,
             rlang::parse_exprs(unlist(all_vars[[new_role]])),
             new_role = new_role)
      do.call(add_role, params)
    }
    
    return(recipe_object)
  }