
#' Create Table One
#'
#' Creates Table One using the tableone package if a bllflow object is passed a custom function
#' then extracts the necessary data for tableone call from the object avoiding unnecessary arguments.
#'
#' @param x Object to retrive variables from
#' @param ... Additional arguments to pass to the tableone::CreateTableOne function call
#' @export
CreateTableOne <- function(x = NULL, ...) {
  UseMethod("CreateTableOne", x)
}

#' Create Table One using bllflow Object
#'
#' Creates table one using the information present in the passed bllflow object
#' specifically uses working data as the data,
#' additional arguments can be passed to create a specific table one.
#' However if no optional args are passed the variable info stored in variables MSW is used.
#'
#' @param x The bllflow object
#' @param vars The optional vars to use in creation of tableone if no vars are passed then vars in MSW variables is used
#' @param strata The optional strata to use in creation of tableone if no strata is passed no strata is used
#' @param factor_vars The optional factor_vars (categorical variables) used in creation of tableone if nothing is passed
#' the MSW variables sheet is used to determine variable types
#' @param select_role The optional argument that specifies the role of the variables to pass to tableone
#' @param ... Additional arguments to pass to the tableone::CreateTableOne function call
#'
#' @return returns a table one \href{https://cran.r-project.org/web/packages/tableone/index.html}{tableone} object
#'
#' @importFrom tableone CreateTableOne
#' @importFrom sjlabelled get_labels
#' @export
CreateTableOne.BLLFlow <- function(x,
                                   vars = NULL,
                                   strata = NULL,
                                   factor_vars = NULL,
                                   select_role = NULL, ...) {
  bll_flow_model <- x
  # ----Step 1: pull from variables in bll_flow_model ----
  variables_sheet <-
    bll_flow_model[[pkg.globals$bllFlowContent.Variables]]
  if (is.null(vars)) {
    if (is.null(select_role)) {
      vars <-
        as.character(variables_sheet[[pkg.globals$MSW.Variables.Columns.Variable]])
    } else {
      vars <-
        as.character(variables_sheet[grepl(select_role, variables_sheet[[pkg.globals$argument.Role]]), pkg.globals$MSW.Variables.Columns.Variable])
    }
    vars <- trimws(vars)
  }
  if (is.null(factor_vars)) {
    if (is.null(select_role)) {
      factor_vars <-
        as.character(variables_sheet[is_equal(variables_sheet[[pkg.globals$MSW.Variables.Columns.VariableType]], pkg.globals$ddiValueName.Categorical), pkg.globals$MSW.Variables.Columns.Variable])
    } else {
      factor_vars <-
        as.character(variables_sheet[is_equal(variables_sheet[[pkg.globals$MSW.Variables.Columns.VariableType]], pkg.globals$ddiValueName.Categorical) && grepl(select_role, variables_sheet[[pkg.globals$argument.Role]]), pkg.globals$MSW.Variables.Columns.Variable])
    }
    factor_vars <- trimws(factor_vars)
  }

  # ----Step 2: Create the tableone ----
  if (is.null(strata)) {
    final_table <-
      tableone::CreateTableOne(
        data = bll_flow_model[[pkg.globals$bllFlowContent.WorkingData]],
        vars = vars,
        factorVars = factor_vars, ...
      )
  } else {
    final_table <-
      tableone::CreateTableOne(
        data = bll_flow_model[[pkg.globals$bllFlowContent.Data]],
        vars = vars,
        factorVars = factor_vars,
        strata = strata, ...
      )
  }
  # Appends valLabels to metadata of tableone for val printing
  valLabels <- sjlabelled::get_labels(bll_flow_model[[pkg.globals$bllFlowContent.WorkingData]][vars], values = "n")
  final_table$MetaData[["valLabels"]] <- valLabels
  return(final_table)
}

#' @export
CreateTableOne.default <- function(...) {
  tableone::CreateTableOne(...)
}

# ---------- WIP Needs further testing ----------
#' #' Summary Data Long Table
#' #'
#' #' Creates a Long table to summarise data from multiple tables in one convenient table.
#' #' Its primary use is to convert Table one tables into a long table.
#' #' The optional arguments allow appending to long table as well as addition of labels
#' #'
#' #' @param table_one the table one object to be converted into a long table
#' #' @param long_table the optional long table to append the table one information to
#' #' @param bll_flow_model The optional bllFlow object containing labels and extra information on the variables
#' #' @return Returns the long table or the bll_flow_model with long table attached
#' #'
#' #' @examples
#' #' library(survival)
#' #' data(pbc)
#' #' pbc$exp_percentile <- runif(nrow(pbc), 0, 1)
#' #' pbc$ageGroup <- ifelse(pbc$age < 20, 1,
#' #' ifelse(pbc$age >= 20 & pbc$age < 40, 2,
#' #' ifelse(pbc$age >= 40 & pbc$age < 80, 3,
#' #' ifelse(pbc$age >= 80, 4, NA))))
#' #'
#' #' library(bllflow)
#' #' variables_sheet <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' #' variable_details <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #' ddi <- read_DDI(file.path(getwd(), 'bllflow/extdata'),"pbcDDI.xml")
#' #' pbc_model <- BLLFlow(pbc, variables_sheet, variable_details, ddi)
#' #'
#' #' pbc_table_one <- CreateTableOne(pbc_model, strata = "edema")
#' #' pbc_summary_table_no_labels <- summary_data_long(pbc_table_one)
#' #' pbc_long_table_with_label <- summary_data_long(pbc_table_one, bll_flow_model = pbc_model, long_table = pbc_summary_table_no_labels)
#' #'@export
#' summary_data_long <-
#'   function(table_one,
#'            long_table = NULL,
#'            bll_flow_model = NULL) {
#'     if (is.null(table_one) & is.null(long_table)) {
#'       warning("No table one or long table was passed to summary_data_long",
#'               call. = FALSE)
#'     }
#'     if (is.null(long_table)) {
#'       long_table <- data.frame(stringsAsFactors = FALSE)
#'       long_table[[pkg.globals$LongTable.VariableCategory]] <-
#'         character()
#'       long_table[[pkg.globals$LongTable.Variable]] <- character()
#'       long_table[[pkg.globals$LongTable.Prevalence]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.Frequency]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.NMissing]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.Mean]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.SD]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.Percentile25]] <-  numeric()
#'       long_table[[pkg.globals$LongTable.Percentile75]] <-  numeric()
#'     } else{
#'       long_table <- long_table[[pkg.globals$LongTable.LongTable]]
#'     }
#'     return_table <-
#'       add_to_long_table(table_one, long_table, bll_flow_model[[pkg.globals$bllFlowContent.PopulatedVariableDetails]])
#'     if (!pkg.globals$LongTable.ClassName %in% class(return_table)) {
#'       class(return_table) <-
#'         append(class(return_table), pkg.globals$LongTable.ClassName)
#'     }
#'     return_table <- unique(return_table)
#'     return_summary_data <- list(summary_data = return_table)
#'     class(return_summary_data) <- "SummaryData"
#'
#'     return(return_summary_data)
#'
#'   }
#'
# # Function to create a long table one for one tableOne
# add_to_long_table <-
#   function(passed_table, long_table, variable_details) {
#     # ----Step 1: Populate long table from cont and cat tableone tables ----
#     # Call Cont table extraction if tableOne contains ContTable
#     returned_long_tables <- list()
#     # table_count is used to populate list and avoid list append issues
#     table_count <- 0
#     if (!is.null(passed_table$ContTable)) {
#       dim_names <- attr(passed_table$ContTable, "dimnames")
#       strata_values <- clean_strata_values(dim_names)
#       table_count <- table_count + 1
#       cont_table_long_table <-
#         extract_data_from_cont_table(
#           passed_table$ContTable,
#           attr(
#             passed_table$ContTable,
#             pkg.globals$tableOne.StrataVarName
#           ),
#           strata_values,
#           long_table,
#           variable_details
#         )
#       returned_long_tables[[table_count]] <- cont_table_long_table
#     }
#
#     # Call Cat table extraction if tableOne contains CatTable
#     if (!is.null(passed_table$CatTable)) {
#       dim_names <- attr(passed_table$CatTable, "dimnames")
#       strata_values <- clean_strata_values(dim_names)
#       table_count <- table_count + 1
#       cat_table_long_table <-
#         extract_data_from_cat_table(
#           passed_table$CatTable,
#           attr(
#             passed_table$CatTable,
#             pkg.globals$tableOne.StrataVarName
#           ),
#           strata_values,
#           long_table,
#           variable_details
#         )
#       returned_long_tables[[table_count]] <- cat_table_long_table
#     }
#
#     # ----Step 2: Add any missing columns to the newly created tables----
#     for (table_to_append in returned_long_tables) {
#       for (column_missing in colnames(long_table)) {
#         if (!column_missing %in% colnames(table_to_append)) {
#           table_to_append[[column_missing]] <- NA
#         }
#       }
#       # synchronizing columns to avoid binding issues
#       for (column_missing in colnames(table_to_append)) {
#         if (!column_missing %in% colnames(long_table)) {
#           # in case of zero row table columns need to be declared in columns <- dataType()
#           # Set data type of missing column to type of append table
#           if (nrow(long_table) == 0) {
#             class(long_table[[column_missing]]) <-
#               class(table_to_append[[column_missing]])
#           } else {
#             long_table[[column_missing]] <- NA
#           }
#         }
#       }
#
#       long_table <-
#         rbind(long_table, table_to_append,  stringsAsFactors = FALSE)
#     }
#
#     return(long_table)
#   }
#
# # Create long table from contTable
# extract_data_from_cont_table <-
#   function(cont_table,
#            strata_name,
#            strata_values,
#            long_table,
#            variable_details) {
#     strata_split_name <- character()
#
#     # ----Step 1: Split the strata name into the two variables ----
#     if (!is.null(strata_name)) {
#       strata_split_name <-
#         unlist(strsplit(as.character(strata_name), split = ":"))
#     } else{
#       strata_split_name <- strata_name
#     }
#
#     # ----Step 2: Add columns to long table
#     long_table_rows <- data.frame()
#
#     # loop through each strata columns
#     # ----Step 3: Extract information for each new row of the longtable ----
#     for (strata_index in 1:length(cont_table)) {
#       variables <- (row.names(cont_table[[strata_index]]))
#       for (row in 1:nrow(cont_table[[strata_index]])) {
#         strata_split_values <-
#           unlist(strsplit(as.character(strata_values[[strata_index]]), split = ":"))
#         # extract all the information for that row
#         num <- cont_table[[strata_index]][row, pkg.globals$tableOne.N]
#         n_miss <-
#           cont_table[[strata_index]][row, pkg.globals$tableOne.Miss]
#         row_mean <-
#           cont_table[[strata_index]][row, pkg.globals$tableOne.Mean]
#         row_SD <-
#           cont_table[[strata_index]][row, pkg.globals$tableOne.SD]
#         row_percentile25 <-
#           cont_table[[strata_index]][row, pkg.globals$tableOne.p25]
#         row_percentile75 <-
#           cont_table[[strata_index]][row, pkg.globals$tableOne.p75]
#
#         # create the row to add to tableOne Long
#         group_by_list <- list()
#         if (length(strata_split_name) > 0) {
#           group_by_list <-
#             fill_in_group_by_columns(strata_split_name,
#                                  strata_split_values,
#                                  group_by_list,
#                                  variable_details)
#         }
#
#         # ----Step 4: Create long table row ----
#         long_table_row <- list()
#         long_table_row[[pkg.globals$LongTable.VariableCategory]] <- NA
#         long_table_row[[pkg.globals$LongTable.Variable]] <-
#           variables[[row]]
#         long_table_row[[pkg.globals$LongTable.Prevalence]] <-  NA
#         long_table_row[[pkg.globals$LongTable.Frequency]] <-  num
#         long_table_row[[pkg.globals$LongTable.NMissing]] <-  n_miss
#         long_table_row[[pkg.globals$LongTable.Mean]] <-  row_mean
#         long_table_row[[pkg.globals$LongTable.SD]] <-  row_SD
#         long_table_row[[pkg.globals$LongTable.Percentile25]] <-
#           row_percentile25
#         long_table_row[[pkg.globals$LongTable.Percentile75]] <-
#           row_percentile75
#         long_table_row <- append(long_table_row, group_by_list)
#
#         # ----Step 5: Clean the row
#         for (each_element_index in 1:length(long_table_row)) {
#           # remove empty classes to avoid bind conflicts
#           # example character(0)
#           if (length(long_table_row[[each_element_index]]) == 0) {
#             long_table_row[[each_element_index]] <- NA
#           }
#         }
#
#         # ----Step 6: Add row to the rest of the rows----
#         long_table_rows <-
#           rbind(long_table_rows, long_table_row,  stringsAsFactors = FALSE)
#       }
#     }
#
#     return(long_table_rows)
#   }
#
# # Create long table from CatTable
# extract_data_from_cat_table <-
#   function(cat_table,
#            strata_name,
#            strata_values,
#            long_table,
#            variable_details) {
#     # ----Step 1: Split the strata name into the two variables ----
#     variables_checked <- 0
#     var_names <- attr(cat_table[[1]], "names")
#     strata_split_name <-
#       unlist(strsplit(as.character(strata_name), split = ":"))
#     # Adds group by columns not found in the long table
#
#     # ----Step 2: Add columns to long table
#     long_table_rows <- data.frame()
#
#     # ----Step 3: Extract information for each new row of the longtable ----
#     for (strata_counter in 1:length(cat_table)) {
#       strata_split_values <-
#         unlist(strsplit(as.character(strata_values[[strata_counter]]), split = ":"))
#       # Loop through the tables of each variable
#       for (selected_variable_table in cat_table[[strata_counter]]) {
#         # Used to specify the variable being writen
#         variables_checked <- variables_checked + 1
#
#         # Loop through the levels of each variable
#         for (row in 1:nrow(selected_variable_table)) {
#           n_miss <- selected_variable_table[row, pkg.globals$tableOne.Miss]
#           frequency <-
#             selected_variable_table[row, pkg.globals$tableOne.Freq]
#           lev_name <-
#             selected_variable_table[row, pkg.globals$tableOne.Level]
#           prevalence <-
#             selected_variable_table[row, pkg.globals$tableOne.Percent]
#           group_by_list <- list()
#           if (length(strata_split_name) > 0) {
#             group_by_list <-
#               fill_in_group_by_columns(strata_split_name,
#                                    strata_split_values,
#                                    group_by_list,
#                                    variable_details)
#             if (!is.null(variable_details)) {
#               group_by_list[[pkg.globals$LongTable.VariableCategoryLabel]] <-
#                 variable_details[is_equal(variable_details[[pkg.globals$argument.VariableStart]], var_names[[variables_checked]]) &
#                                   is_equal(variable_details[[pkg.globals$argument.CatStartValue]], as.character(lev_name)), pkg.globals$argument.CatStartLabel]
#               # If empty add NA
#               if (length(group_by_list[[pkg.globals$LongTable.VariableCategoryLabel]]) == 0) {
#                 group_by_list[[pkg.globals$LongTable.VariableCategoryLabel]] <- NA
#               }
#             }
#           }
#
#           # ----Step 4: Create long table row ----
#           long_table_row <- list()
#           long_table_row[[pkg.globals$LongTable.VariableCategory]] <-
#             lev_name
#           long_table_row[[pkg.globals$LongTable.Variable]] <-
#             var_names[variables_checked]
#           long_table_row[[pkg.globals$LongTable.Prevalence]] <-
#             prevalence
#           long_table_row[[pkg.globals$LongTable.Frequency]] <-
#             frequency
#           long_table_row[[pkg.globals$LongTable.NMissing]] <-  n_miss
#           long_table_row[[pkg.globals$LongTable.Mean]] <-  NA
#           long_table_row[[pkg.globals$LongTable.SD]] <-  NA
#           long_table_row[[pkg.globals$LongTable.Percentile25]] <-  NA
#           long_table_row[[pkg.globals$LongTable.Percentile75]] <-  NA
#           long_table_row <- append(long_table_row, group_by_list)
#
#           # ----Step 5: Clean the row
#           for (each_element_index in 1:length(long_table_row)) {
#             if (length(long_table_row[[each_element_index]]) == 0) {
#               long_table_row[[each_element_index]] <- NA
#             }
#           }
#
#           # ----Step 6: Add row to the rest of the rows----
#           long_table_rows <-
#             rbind(long_table_rows, long_table_row,  stringsAsFactors = FALSE)
#         }
#       }
#       variables_checked <- 0
#     }
#
#     return(long_table_rows)
#   }
