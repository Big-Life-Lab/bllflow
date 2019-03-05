#' Check for Small Cells
#'
#' Small Cells Check checks a given table for small sells then adds a
#' smallCells table to the MetaData of the table object
#'
#' Checks the categorical table within the TableOne param (CatTable field) for
#' small cells. A small cell is a category where the number of people
#' in the category (n) is less than the value specified by the smallSize param.
#' The freq field within each variable has the n values.
#'
#' @param passedTable The object outputted by the CreateTableOne function of the tableone package.
#' The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param smallSize What value constitutes a small size cell. Default value is 6.
#' @param print If TRUE prints the smallSize metadata in a human readable format
#' @param tableType Specifies the type of the table that is passed to the function
#'
#' @return The passedTable object with a new object in the Metadata object called smallCells.
#' smallCells is a dataframe with 4 columns
#' stratifiedBy : the categeries the table was stratified by
#' strataValues : the strata value where the small cell is present
#' variableName and factors and the rows are all the categorical variables
#' whose one or more factors have small cells.
#'
#'
#' @export
CheckSmallCells <- function(passedTable,
                            smallSize = 6,
                            print = TRUE,
                            tableType = "TableOne") {
  # Chosing Table procesing function -------------------------------------------

  # Handles TableOne type tables
  if (tableType == "TableOne") {
    smallSizeTable <- CheckSmallCellsInTableOne(passedTable, smallSize)
    # In case an unsupported table type is used this error is thrown
  } else {
    stop(
      cat(
        "Table type ",
        tableType,
        " is not a valid table type or is not yet supported "
      ),
      "Unsupported Type"
    )
  }

  # Outputing the created Table function ---------------------------------------

  # Writes the created table into the MetaData object of the passed table
  # Appends to smallCells if previous reccord exists
  if ("smallCells" %in% names(passedTable$MetaData)) {
    passedTable$MetaData[["smallCells"]] <-
      rbind(passedTable$MetaData$smallCells, smallSizeTable)
  } else {
    passedTable$MetaData[["smallCells"]] <- smallSizeTable
  }
  # Prints the table if the print is requested
  if (print) {
    print(passedTable$MetaData$smallCells)
  }
  return(passedTable)
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
#' @param tableOne The object outputted by the CreateTableOne function of the
#' tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param smallSize What value constitutes a small size cell. Default value is 6.
#'
#' @return data frame with 4 columns: stratifiedBy, strataValues, variableName, factors.
#' This only adds the variables that contain small cells for easy identification.
#' It returns an empty table when no small cells are present
CheckSmallCellsInTableOne <- function(tableOne,
                                      smallSize = 6) {
  # Variable declaration -------------------------------------------------------

  variablesChecked <- 0
  levelsChecked <- 0
  variablesFound <- 0
  levelsFound <- 0
  smallCellFound <- FALSE
  varNames <- attr(tableOne$CatTable[[1]], "names")
  counter <- 1
  freqVector <- character()
  dimNames <- attr(tableOne$CatTable, "dimnames")
  strataCounter <- 1
  strataAllCombinationsDataFrame <- expand.grid(dimNames)
  strataArgs <- c(strataAllCombinationsDataFrame, sep = ":")
  strataValues <- do.call(paste, strataArgs)
  # Inserts values if not stratified
  if (is.null(attr(tableOne$CatTable, "strataVarName"))) {
    stratifiedBy <- "No Strat"
    strataValues <- "No Strat"
  } else {
    stratifiedBy <- attr(tableOne$CatTable, "strataVarName")
  }
  # Creates a first row in a data frame
  # due to rbind function not working on an empty dataframe
  # A dummy row is used because first row is unknown at time of creation
  detectedSmallCells <-
    data.frame(
      stratifiedBy = "DummyRow",
      strataValues = "DummyRow",
      variableName = "DummyRow"
    )
  detectedSmallCells$factors <- list(c(1, 2, 3))
  print(strataValues)
  # Small Cell detection -------------------------------------------------------



  # Loop through the tables for each column
  for (i in tableOne$CatTable) {
    # Loop through the tables of each variable
    for (j in i) {
      variablesChecked <- variablesChecked + 1
      # Loop through the levels of each variable
      for (row in 1:nrow(j)) {
        levelsChecked <- levelsChecked + 1
        frequent <- j[row, "freq"]
        levName <- j[row, "level"]
        if (frequent < smallSize) {
          smallCellFound <- TRUE
          levelsFound <- levelsFound + 1
          freqVector <- c(freqVector, levName)
        }
      }
      if (smallCellFound) {
        variablesFound <- variablesFound + 1
        # Creates a temporary dataframe with data for the table that was read
        # Then that dataframe is added
        tmp <-
          data.frame(
            stratifiedBy = stratifiedBy,
            strataValues = strataValues[[strataCounter]],
            variableName = varNames[counter]
          )
        tmp$factors <- list(freqVector)
        detectedSmallCells <- rbind(detectedSmallCells, tmp)
        smallCellFound <- FALSE
      }
      counter <- counter + 1
      freqVector <- NULL
    }
    counter <- 1
    strataCounter <- strataCounter + 1
  }
  # Removes the dummy row from the return dataframe and resets the counters
  cat(
    variablesChecked,
    " variables with ",
    levelsChecked,
    " levels checked.\n\n"
  )
  cat(
    variablesFound,
    " variables with ",
    levelsFound,
    " levels have cells <",
    smallSize,
    " counts.\n\n"
  )
  detectedSmallCells <- detectedSmallCells[-c(1), ]
  rownames(detectedSmallCells) <- NULL
  return(detectedSmallCells)
}
