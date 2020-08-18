# ---------- WIP REQUIRES SUMMARY TABLE REVISIT ----------
#' #' Create Bll Model Object
#' #'
#' #' This object is used to generate the PMML file, for manuscript figures and other uses.
#' #'
#' #' @param model_object The object that is returned when a model is created.
#' #' @param model_type values = crr, NULL. The class name of the model_object. "crr" is the class name for the Fine and Grey model. This is currently the only model that is supported.
#' #' @param table_one The object returned by createTableOne().
#' #' @param model_data The data used to generate the model.
#' #' @param calculate_mean default = TRUE. If the means should be included in the table
#' #' @param baseline_risk_time_frame default = 5. The time for the baseline risk make sure to only use years as input
#' #' @export
#' create_BLL_model_object <-
#'   function(model_data,
#'            model_object,
#'            table_one = NULL,
#'            model_type = NULL,
#'            calculate_mean = TRUE,
#'            baseline_risk_time_frame = 5) {
#'     # ----Step 1: verify input/create not passed input----
#'     supported_model_types <- c("crr")
#'     var_names <- attr(model_object$coef, "names")
#'
#'     if (!class(model_object) %in% supported_model_types) {
#'       stop("Passed model type is not yet supported. Aborting!")
#'     }
#'     if (is.null(table_one)) {
#'       table_one <-
#'         tableone::CreateTableOne(data = model_data, vars = var_names)
#'     } else {
#'       for (var_name in var_names) {
#'         if (!var_name %in% table_one[[pkg.globals$LongTable.MetaData]][[pkg.globals$tableOne.Vars]]) {
#'           # Issue warning before creating table one
#'           warning(
#'             "Passed table one does not contain the vars in the passed model. Creating new TableOne \n"
#'           )
#'           # Verify data contains the var_names
#'           var_in_data <- var_names %in% colnames(model_data)
#'           if (all(var_in_data)) {
#'             table_one <-
#'               tableone::CreateTableOne(data = model_data, vars = var_names)
#'           } else {
#'             stop("The model_data does not contain all the variables from the model. Aborting!")
#'           }
#'           break()
#'         }
#'       }
#'     }
#'
#'     # ----Step 2: Generate model object ----
#'     # Obtain the beta coefficient
#'     beta_coefficient <- model_object$coef
#'     all_strata_var_means <- list()
#'     ret_table <-
#'       data.frame(beta_coefficient = beta_coefficient, row.names = var_names)
#'
#'     # Obtain the means
#'     if (calculate_mean) {
#'       if (!is.null(table_one$ContTable)) {
#'         for (strataVar in length(table_one$ContTable)) {
#'           all_strata_var_means[[strataVar]] <-
#'             table_one$ContTable[[strataVar]][var_names, pkg.globals$tableOne.Mean]
#'           ret_table[[pkg.globals$tableOne.Mean]] <-
#'             all_strata_var_means[[strataVar]]
#'         }
#'       } else {
#'         warning("The table_one does not contain cont table therefore means were not calculated")
#'       }
#'     }
#'     baseline_risk <- calculate_baseline_risk(model_object, (365.25*baseline_risk_time_frame))
#'
#'     return(list(reference = ret_table, baseline = baseline_risk))
#'   }
#'
#' calculate_baseline_risk <- function(model, time) {
#'   jumps <- data.frame(time = model$uftime, bfitj = model$bfitj)
#'   jumps_time <- jumps[jumps$time <= time, ]
#'   b0 <- sum(jumps_time$bfitj)
#'   out <- 1-exp(-b0)
#'   return(out)
#' }
