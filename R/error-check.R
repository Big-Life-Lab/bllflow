check_for_column_presence <- function(names, frame, frame_name) {
  missing_col_names <- names[names %in% colnames(frame) == FALSE]
  if (length(missing_col_names) != 0) {
    stop(paste(
      "Column(s)",
      missing_col_names,
      "are missing from ",
      frame_name,
      "\n"
    ))
  }
}

check_for_existance_of_in_list <- function(names, passed_list, list_name) {
  for (name.check_row in names) {
    if (!(name.check_row %in% names(passed_list))) {
      stop(paste("The", name.check_row, "is missing from the", list_name))
    }
  }
}

check_if_data_frame <- function(passed_frame, passed_name) {
  if (!is.data.frame(passed_frame)) {
    stop(paste("The ", passed_name, " object is not a data frame"))
  }
}

check_if_cell_is_empty <-
  function(cell_content,
             row_number,
             column_name,
             ddi_value) {
    is_empty <- TRUE
    if (!is.null(cell_content) &&
      !is.na(cell_content) &&
      cell_content != "" && cell_content != ddi_value) {
      warning(
        paste(
          "Row ",
          row_number,
          ":",
          column_name,
          " column has value \"",
          cell_content,
          "\" but DDI value is \"",
          ddi_value,
          "\". Not overwriting"
        ),
        call. = FALSE,
        immediate. = TRUE
      )
      is_empty <- FALSE
    }

    return(is_empty)
  }
