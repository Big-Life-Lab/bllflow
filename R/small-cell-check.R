#' check_small_cells
#'
#' Checks for presence of small cells within the passed table
#'
#' @param passed_table Table to check currently supported is LongTable and TableOne
#' @param small_size Preffered small cell size default <6
#' @param print Option to print the smallCell table
#' @return Returns passed table with smallcells attached inside MetaData$smallCells
#' @export
check_small_cells <- function(passed_table, ...) {
  UseMethod("check_small_cells", passed_table)
}
#' check_small_cells for Summary Data
#'
#' Checks for presence of small cells within Summary Data
#'
#' @param passed_table Table to check currently supported is LongTable and TableOne
#' @param small_size Preffered small cell size default <6
#' @param print Option to print the smallCell table
#' @return Returns passed table with smallcells attached inside MetaData$smallCells
#'@export
check_small_cells.SummaryData <- function(passed_table,
                                          small_size = 6,
                                          print = FALSE) {
  passed_table[[pkg.globals$LongTable.MetaData]][[pkg.globals$LongTable.SmallCells]] <-
    passed_table$summaryData[passed_table$summaryData[, pkg.globals$LongTable.Frequency] < small_size,]
  print(paste(nrow(passed_table[[pkg.globals$LongTable.MetaData]][[pkg.globals$LongTable.SmallCells]]), "Small cells were found"))
  if (print) {
    print(passed_table[[pkg.globals$LongTable.MetaData]][[pkg.globals$LongTable.SmallCells]])
  }
  
  return(passed_table)
}

#' Check for Small Cells
#'
#' Small Cells Check checks a given table for small sells then adds a
#' smallCells table to the MetaData of the table object
#'
#' Checks the categorical table within the TableOne param (CatTable field) for
#' small cells. A small cell is a category where the number of people
#' in the category (n) is less than the value specified by the small_size param.
#' The freq field within each variable has the n values.
#'
#' @param passed_table The object outputted by the CreateTableOne function of the tableone package.
#' The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param small_size What value constitutes a small size cell. Default value is 6.
#' @param print If TRUE prints the small_size metadata in a human readable format
#' @param table_type Specifies the type of the table that is passed to the function
#'
#' @return The passed_table object with a new object in the Metadata object called smallCells.
#' smallCells is a dataframe with 4 columns
#' stratified_by : the categeries the table was stratified by
#' strata_values : the strata value where the small cell is present
#' variableName and factors and the rows are all the categorical variables
#' whose one or more factors have small cells.
#'
#' @examples
#'
#' # Read in the data we will use to generate Table One
#'
#' library(survival)
#' data(pbc)
#'
#' # Create the Table One object
#'
#' library("tableone")
#' # The list of variables which are categorical
#' catVars <- c("status", "trt", "ascites", "hepato",
#'              "spiders", "edema", "stage")
#'
#' # create table 1 object
#' TableOne <- CreateTableOne(data = pbc,strata = c("trt","stage"), factorVars = catVars)
#'
#' # by default small_size is 6 print is set to true and table_type is TableOne
#' tmp <- check_small_cells(TableOne)
#'
#' # increasing the small_size threshold to 10
#' tmp <- check_small_cells(TableOne, small_size=10)
#'
#' # currently only TableOne is supported so table_type != TableOne will throw error
#' #tmp <- check_small_cells(TableOne, table_type="TableTwo")
#'
#' @export
check_small_cells.TableOne <- function(passed_table,
                                       small_size = 6,
                                       print = FALSE,
                                       table_type = "TableOne") {
  # Chosing Table procesing function -------------------------------------------
  
  # Handles TableOne type tables
  if (table_type == "TableOne") {
    small_size_table <-
      check_small_cells_in_table_one(passed_table, small_size)
    # In case an unsupported table type is used this error is thrown
  } else {
    stop(
      cat(
        "Table type ",
        table_type,
        " is not a valid table type or is not yet supported "
      ),
      "Unsupported Type"
    )
  }
  
  # Outputing the created Table function ---------------------------------------
  
  # Writes the created table into the MetaData object of the passed table
  # Appends to smallCells if previous reccord exists
  if ("smallCells" %in% names(passed_table$MetaData)) {
    passed_table$MetaData$smallCells <-
      rbind(passed_table$MetaData$smallCells, small_size_table)
    # Sort the small cells table by variable, stratified_by, strata_values
    passed_table$MetaData$smallCells <-
      passed_table$MetaData$smallCells[order(
        passed_table$MetaData$smallCells$variable,
        passed_table$MetaData$smallCells$stratified_by,
        passed_table$MetaData$smallCells$strata_values
      ), ]
    # reset rowcount
    rownames(passed_table$MetaData$smallCells) <- NULL
  } else {
    # Sort the small cells table by variable, stratified_by, strata_values
    passed_table$MetaData$smallCells <-
      small_size_table[order(
        small_size_table$variable,
        small_size_table$stratified_by,
        small_size_table$strata_values
      ), ]
    # reset rowcount
    rownames(passed_table$MetaData$smallCells) <- NULL
  }
  # Prints the table if the print is requested
  if (print) {
    if (nrow(passed_table$MetaData$smallCells) == 0) {
      cat("No small cells are present")
    } else{
      print(passed_table$MetaData$smallCells)
    }
  }
  
  return(passed_table)
}

# Table Parsing Functions -----------------------------------------------------------------------

#' Check for Small Cells in TableOne
#'
#' Check for Small Cells inside a TableOne format Table
#'
#' Check The CatTable list for all possible small cells this also checks for
#' all levels similar to showAllLevels in the CatTable from TableOne
#' documentation available here:
#'
#' @param table_one The object outputted by the CreateTableOne function of the
#' tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param small_size What value constitutes a small size cell. Default value is 6.
#'
#' @return data frame with 4 columns: stratified_by, strata_values, variableName, factors.
#' This only adds the variables that contain small cells for easy identification.
#' It returns an empty table when no small cells are present
check_small_cells_in_table_one <- function(table_one,
                                      small_size = 6) {
  # Variable declaration -------------------------------------------------------
  
  strata_checked <- 0
  levels_checked <- 0
  variables_found <- 0
  levels_found <- 0
  variables_checked_num <- 0
  small_cell_found <- FALSE
  var_names <- attr(table_one$CatTable[[1]], "names")
  counter <- 1
  freq_vector <- character()
  dim_names <- attr(table_one$CatTable, "dimnames")
  strata_counter <- 1
  # This turns the strata arrays into one single array
  # Then creates all possible combinations and seperates them with :
  # Then all the combinations are combined into a single string array
  strata_all_combinations_data_frame <- expand.grid(dim_names)
  strata_args <- c(strata_all_combinations_data_frame, sep = ":")
  strata_values <- do.call(paste, strata_args)
  # Inserts values if not stratified
  if (is.null(attr(table_one$CatTable, "strataVarName"))) {
    stratified_by <- NA
    strata_values <- NA
  } else {
    stratified_by <- attr(table_one$CatTable, "strataVarName")
  }
  # Creates a first row in a data frame
  # due to rbind function not working on an empty dataframe
  # A dummy row is used because first row is unknown at time of creation
  detected_small_cells <-
    data.frame(
      variable = character(),
      stratified_by = character(),
      strata_values = character()
    )
  detected_small_cells$factors <- list()
  # Small Cell detection -------------------------------------------------------
  # Loop through the tables for each column
  for (strata_counter in 1:length(table_one$CatTable)) {
    variables_checked_num <- 0
    # Loop through the tables of each variable
    for (selected_variable in table_one$CatTable[[strata_counter]]) {
      variables_checked_num <- variables_checked_num + 1
      strata_checked <- strata_checked + 1
      # Loop through the levels of each variable
      for (row in 1:nrow(selected_variable)) {
        levels_checked <- levels_checked + 1
        frequency <- selected_variable[row, "freq"]
        lev_name <- selected_variable[row, "level"]
        if (frequency < small_size) {
          small_cell_found <- TRUE
          levels_found <- levels_found + 1
          freq_vector <- c(freq_vector, lev_name)
        }
      }
      if (small_cell_found) {
        variables_found <- variables_found + 1
        # Creates a temporary dataframe with data for the table that was read
        # Then that dataframe is added
        new_small_cell_row <-
          data.frame(
            variable = var_names[counter],
            stratified_by = stratified_by,
            strata_values = strata_values[[strata_counter]]
          )
        new_small_cell_row$factors <- list(freq_vector)
        detected_small_cells <-
          rbind(detected_small_cells, new_small_cell_row)
        small_cell_found <- FALSE
      }
      counter <- counter + 1
      freq_vector <- NULL
    }
    counter <- 1
  }
  cat(variables_checked_num,
      " variables with ",
      levels_checked,
      " levels checked.\n\n")
  cat(
    length(levels(detected_small_cells$variable)),
    " variables with ",
    levels_found,
    " levels have cells <",
    small_size,
    " counts.\n\n"
  )
  return(detected_small_cells)
}
