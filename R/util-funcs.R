# Function to compare, even with NA present
# This function returns TRUE wherever elements are the same, including NA's,
# and false everywhere else.
is_equal <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instanses of NA with FALSE
  same[is.na(same)] <- FALSE

  return(same)
}


# Cleans strata values
clean_strata_values <-
  function(dim_names) {
    strata_all_combinations_data_frame <- expand.grid(dim_names)
    strata_args <- c(strata_all_combinations_data_frame, sep = ":")
    strata_values <- do.call(paste, strata_args)

    return(strata_values)
  }

# ---------- WIP PART OF SUMMARY TABLE ----------
# # Adds the column to the list as well as the dataframe that is passed
# add_column <-
#   function(column_name,
#            table_to_add_to) {
#     if (!column_name %in% colnames(table_to_add_to)) {
#       if (nrow(table_to_add_to) == 0) {
#         table_to_add_to[, column_name] <- character()
#       } else {
#         table_to_add_to[, column_name] <- NA
#       }
#     }
#
#     return(table_to_add_to)
#   }
#
# # Adds groupBy columns to long table
# add_group_by_columns <-
#   function(strata_split_name,
#            long_table,
#            variable_details) {
#     for (group_by_index in 1:length(strata_split_name)) {
#       long_table <-
#         add_column(paste(pkg.globals$LongTable.GroupBy, group_by_index, sep = ""),
#                    long_table)
#       long_table <-
#         add_column(paste(pkg.globals$LongTable.GroupByValue, group_by_index, sep = ""),
#                    long_table)
#
#       if (!is.null(variable_details)) {
#         long_table <-
#           add_column(paste(pkg.globals$LongTable.GroupByLabel, group_by_index, sep = ""),
#                      long_table)
#         long_table <-
#           add_column(
#             paste(
#               pkg.globals$LongTable.GroupByValueLabel,
#               group_by_index,
#               sep = ""
#             ),
#             long_table
#           )
#       }
#     }
#
#     return(long_table)
#   }
#
# # Fills group by columns with information from variable details
# fill_in_group_by_columns <-
#   function(strata_split_name,
#            strata_split_values,
#            long_table_row,
#            variable_details) {
#     for (group_by_index in 1:length(strata_split_name)) {
#       long_table_row[[paste(pkg.globals$LongTable.GroupBy, group_by_index, sep = "")]] <-
#         strata_split_name[[group_by_index]]
#       long_table_row[[paste(pkg.globals$LongTable.GroupByValue, group_by_index, sep = "")]] <-
#         strata_split_values[[group_by_index]]
#
#       if (!is.null(variable_details)) {
#         long_table_row[[paste(pkg.globals$LongTable.GroupByLabel, group_by_index, sep = "")]] <-
#           variable_details[is_equal(variable_details[[pkg.globals$argument.VariableStart]], strata_split_name[[group_by_index]]) &
#                              is_equal(variable_details[[pkg.globals$argument.CatStartValue]], strata_split_values[[group_by_index]]), pkg.globals$argument.VariableStartLabel]
#         long_table_row[[paste(pkg.globals$LongTable.GroupByValueLabel,
#                               group_by_index,
#                               sep = "")]] <-
#           variable_details[is_equal(variable_details[[pkg.globals$argument.VariableStart]], strata_split_name[[group_by_index]]) &
#                              is_equal(variable_details[[pkg.globals$argument.CatStartValue]], strata_split_values[[group_by_index]]), pkg.globals$argument.CatStartLabel]
#
#       }
#     }
#
#     return(long_table_row)
#   }
