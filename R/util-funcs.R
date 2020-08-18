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
# Fills group by columns with information from variable details
