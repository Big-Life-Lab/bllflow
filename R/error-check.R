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


check_if_data_frame <- function(passed_frame, passed_name) {
  if (!is.data.frame(passed_frame)) {
    stop(paste("The ", passed_name, " object is not a data frame"))
  }
}


