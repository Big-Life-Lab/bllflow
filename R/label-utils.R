#' Generic label setting function
#'
#' When a bllflow object is passed the information in the object
#' is used to label the data.
#' The default handles non bllflow labeling and requires additional params
#'
#' @param x bllflow or data being labeled
#' @param ... Used for generic function consistency
#'
#' @return for bllflow versions a bllflow object with labelled
#' working_data is returned for default a data.frame with labels is returned
#' @export
set_data_labels <- function(x = NULL, ...) {
  UseMethod("set_data_labels", x)
}

#' @export
set_data_labels.BLLFlow <- function(x, ...) {
  bllflow_object <- x
  ret <-
    set_data_labels.default(
      bllflow_object$working_data,
      variable_details = bllflow_object$variable_details,
      variables_sheet = bllflow_object$variables
    )

  bllflow_object$working_data <- ret

  return(bllflow_object)
}

#' @title Set Data Labels
#' @description sets labels for passed database, Uses the names of final
#' variables in variable_details/variables_sheet as well as the labels contained
#' in the passed dataframes
#'
#' @param x newly transformed dataset
#' @param variable_details this is for non bllflow and
#' contains details about variable category labels
#' @param variables_sheet this is for non bllflow and
#' contains details about variable labels
#' @param ... Used for generic function consistency
#'
#' @return labeled data_to_label
#'
#' @export
set_data_labels.default <-
  function(x,
             variable_details,
             variables_sheet = NULL, ...) {
    data_to_label <- x
    # Extract variables in the data
    variable_names <- unique(colnames(data_to_label))
    # extract only relevant variable info
    if (!is.null(variable_details)) {
      variable_details[[pkg.globals$argument.Variables]] <- 
        sapply(variable_details[[pkg.globals$argument.Variables]], trimws)
      variable_details <-
        variable_details[variable_details[[pkg.globals$argument.Variables]]
        %in% variable_names, ]
      if (is.null(variables_sheet)) {
        # In case variables sheet is missing to avoid null pointer error empty columns are created
        variable_details[[pkg.globals$MSW.Variables.Columns.Label]] <- NA
        variable_details[[pkg.globals$MSW.Variables.Columns.LabelLong]] <-
          NA
      }
    }
    if (!is.null(variables_sheet)) {
      variables_sheet[[pkg.globals$argument.Variables]] <- 
        sapply(variables_sheet[[pkg.globals$argument.Variables]], trimws)
      variables_sheet <-
        variables_sheet[variables_sheet[[pkg.globals$argument.Variables]] %in%
          variable_names, ]
      variable_details <-
        update_variable_details_based_on_variable_sheet(
          variable_sheet = variables_sheet,
          variable_details = variable_details
        )
      # Add rows from variables that are missing from variable_details
      var_details_names <- 
        unique(variable_details[[pkg.globals$argument.Variables]])
      # Captures variables not in variable details
      missing_vars <- variable_names[(!variable_names %in% var_details_names)]
      missing_vars <- 
        missing_vars[missing_vars %in% variables_sheet[[
          pkg.globals$argument.Variables]]]
      # Missing variables in variables
      catch_up_vars <-
        variables_sheet[variables_sheet[[
          pkg.globals$argument.Variables]] %in% missing_vars, ]
      keep_columns <-
        c(
          pkg.globals$argument.Variables,
          pkg.globals$argument.CatLabel,
          pkg.globals$argument.CatLabelLong,
          pkg.globals$argument.ToType,
          pkg.globals$argument.VariableLabelShort,
          pkg.globals$argument.VariableLabel,
          pkg.globals$argument.Units,
          pkg.globals$argument.recEnd
        )
      if (nrow(catch_up_vars) > 0) {
        # Setting any missing columns to NA to allow for rbind
        catch_up_vars[keep_columns[
          (!keep_columns %in% colnames(catch_up_vars))]] <-
          NA
        # Vars not in variable details can only be cont
        catch_up_vars[[pkg.globals$argument.ToType]] <- "cont"
        variable_details <- variable_details[keep_columns]
        catch_up_vars <- catch_up_vars[keep_columns]
        # Append any missing vars
        variable_details <- rbind(variable_details, catch_up_vars)
      }
    }
    label_list <- NULL
    for (variable_name in variable_names) {
      rows_to_process <-
        variable_details[variable_details[[pkg.globals$argument.Variables]] ==
                           variable_name, ]
      label_list[[variable_name]] <-
        create_label_list_element(rows_to_process)
    }
    data_to_label <- label_data(label_list, data_to_label)

    return(data_to_label)
  }

create_label_list_element <- function(variable_rows) {
  ret_list <- list(
    # Variable type
    type = NULL,
    # Variable value units
    unit = NULL,
    # variable label long
    label_long = NULL,
    # variable label
    label = NULL,
    # Variable value label
    values = c(),
    # Variable value label long
    values_long = c()
  )
  first_row <- variable_rows[1, ]
  ret_list$type <-
    as.character(first_row[[pkg.globals$argument.ToType]])
  ret_list$unit <-
    as.character(first_row[[pkg.globals$argument.Units]])
  ret_list$label_long <-
    as.character(first_row[[pkg.globals$argument.VariableLabel]])
  ret_list$label <-
    as.character(first_row[[pkg.globals$argument.VariableLabelShort]])
  if (is_equal(ret_list$type, pkg.globals$ddiValueName.Cat)) {
    for (row_index in seq_len(nrow(variable_rows))) {
      single_row <- variable_rows[row_index, ]
      # Verify type stays the same
      if (!is_equal(
        ret_list$type,
        as.character(single_row[[pkg.globals$argument.ToType]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.ToType,
            "variable cant change variable type for different values"
          )
        )
      }
      # Verify unit is identical
      if (!is_equal(
        ret_list$unit,
        as.character(single_row[[pkg.globals$argument.Units]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.Units,
            "variable cant change unit type for different values"
          )
        )
      }
      # Verify variable label is identical
      if (!is_equal(
        ret_list$label_long,
        as.character(single_row[[pkg.globals$argument.VariableLabel]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.VariableLabel,
            "variable cant change variableLabel for different values. VAL1:",
            ret_list$label_long,
            "VAL2:",
            as.character(single_row[[pkg.globals$argument.VariableLabel]])
          )
        )
      }
      value_being_labeled <-
        as.character(single_row[[pkg.globals$argument.recEnd]])
      value_being_labeled <-
        recode_variable_NA_formating(value_being_labeled, ret_list$type)
      ret_list$values[[as.character(single_row[[
        pkg.globals$argument.CatLabel]])]] <-
        value_being_labeled
      ret_list$values_long[[as.character(single_row[[
        pkg.globals$argument.CatLabelLong]])]] <-
        value_being_labeled
    }
  }

  return(ret_list)
}

#' @title label_data
#'
#' @description Attaches labels to the data_to_label to preserve metadata
#'
#' @param label_list the label list object that contains extracted labels
#' from variable details
#' @param data_to_label The data that is to be labeled
#' @importFrom sjlabelled set_labels set_label set_label<-
#'
#' @return Returns labeled data
label_data <- function(label_list, data_to_label) {
  for (variable_name in names(label_list)) {
    if (is.na(label_list[[variable_name]]$type)) {
      warning(
        paste(
          variable_name,
          "is missing from variable_details or variables
          (if it was also passed) please verify correct spelling"
        )
      )
      next()
    }
    if (label_list[[variable_name]]$type == pkg.globals$argument.CatType) {
      if (class(data_to_label[[variable_name]]) != "factor") {
        data_to_label[[variable_name]] <-
          factor(data_to_label[[variable_name]])
      }
      data_to_label[[variable_name]] <-
        sjlabelled::set_labels(data_to_label[[variable_name]],
          labels = label_list[[variable_name]]$values
        )
      attr(data_to_label[[variable_name]], "labels_long") <-
        label_list[[variable_name]]$values_long
    } else {
      if (class(data_to_label[[variable_name]]) == "factor") {
        data_to_label[[variable_name]] <-
          as.numeric(levels(data_to_label[[variable_name]])
          [data_to_label[[variable_name]]])
      } else {
        data_to_label[[variable_name]] <-
          as.numeric(data_to_label[[variable_name]])
      }
    }
    sjlabelled::set_label(data_to_label[[variable_name]]) <-
      label_list[[variable_name]]$label
    attr(data_to_label[[variable_name]], "unit") <-
      label_list[[variable_name]]$unit
    attr(data_to_label[[variable_name]], "label_long") <-
      label_list[[variable_name]]$label_long
  }

  return(data_to_label)
}
