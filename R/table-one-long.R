#' Creates a "Table One Long" and stores it in the metadata list.
#' "Table One Long" has the same meaning as a regular table one except it consists
#' of several table one's each of which gives summary statistics on a variable
#' which may or may not be stratified by other variables
#' A tableVariables spreadsheet specified how each table one within the final
#' table should be build. An example is available here
#' https://docs.google.com/spreadsheets/d/1QVqLKy_C185hzeQdJeOy-EeFMBXui1hZ1cB2sKqPG-4/edit#gid=1336039089.
#' An example of the output table is available here
#' https://docs.google.com/spreadsheets/d/1oDcl0Ed-KElO_a_DBWcontVnqyTCZlT63hSPd-gId88/edit#gid=276021298.
#' @param bllFlowModel A bllFlow model
#' @param tableVariablesSheet The data frame with information on how to build
#' the table
#' @return A dataframe containing the table one long
#' @export
#'
#' @examples
#' # Install the packages
#'
#' # Use to generate the table one object
#' install.packages("tableone")
#' # Has the data we will use to generate a table one
#' install.packages("survival")
#'
#' # Read in the data we will use to generate Table One
#'
#' library(survival)
#' data(pbc)
#'
#' # Read in the MSW and variable_details sheet for the PBC model
#' variablesSheet <- read.csv(file.path(getwd(), 'inst/extdata/PBC/PBC - variables.csv'))
#' variableDetailsSheet <- read.csv(file.path(getwd(), 'inst/extdata/PBC/PBC - variable_details.csv'))
#' tableVariablesSheet <- read.csv(file.path(getwd(), 'inst/extdata/PBC/PBC-table-variables.csv'))
#'
#' # Create a bllFlow R object for the PBC model using the above variables as args
#' library(bllFlow)
#' pbcModel <- BLLFlow(pbc, variablesSheet, variableDetailsSheet)
#' tables.CreateTableOneLong(pbcModel,tableVariablesSheet)
#' @import tableone
tables.CreateTableOneLong <- function(bllFlowModel,
                                      tableVariablesSheet) {
  # Extract the needed info from tableVariablesSheet to create the tableOne tables
  variablesForStrata <-
    unique(unlist(tableVariablesSheet[, grepl("pbcSummaryStat", colnames(tableVariablesSheet))]))
  variablesForStrata <- variablesForStrata[variablesForStrata != ""]
  #tablesTB <-
  # lapply(variablesForStrata , function(strataValue)
  #   CreateCustomTableOne(strataValue, tableVariablesSheet, bllFlowModel))
  testTable <-
    CreateCustomTableOne(variablesForStrata[2], tableVariablesSheet, bllFlowModel)
  #print(attributes(tablesTB[[1]]))
  longTable <-
    data.frame(
      groupBy1 = "DummyRow",
      groupByValue1 = "DummyRow",
      groupByLabel1 = "DummyRow",
      groupByValueLabel1 = "DummyRow",
      groupBy2 = "DummyRow",
      groupByValue2 = "DummyRow",
      groupByLabel2 = "DummyRow",
      groupByValueLabel2 = "DummyRow",
      variableCategory = 1,
      variableCategoryLabel = "DummyRow",
      variable = "DummyRow",
      prevalence = 0.25,
      n = 1000,
      nMissing = 1000,
      mean = 100.100,
      sd = 100.100,
      percentile25 = 100.100,
      percentile75 = 100.100
    )
  #print(bllFlowModel$variableDetailsSheet)
  AddToLongTable(testTable, bllFlowModel$variableDetailsSheet)
  #longTable <- rbind(longTable, AddToLongTable(testTable))
  
  #return()
  
}

CreateCustomTableOne <-
  function(strataValue, variableNames, bllFlowModel) {
    test <-
      apply(variableNames, 1, function(row)
        any(row %in% c(as.character(strataValue))))
    tableOneVars <- variableNames$variables[test]
    strataList <-
      unlist(strsplit(as.character(strataValue), split = ", "))
    retTable <-
      CreateTableOne(
        vars = as.character(tableOneVars),
        data = bllFlowModel$data,
        strata = strataList
      )
    #print(attributes(retTable))
    return(retTable)
  }

AddToLongTable <- function(passedTable, variableDetails) {
  # group_by_1 set to first strat
  # group by value 1 set to first strata value
  # group by label 1 set to label of the variable
  # group by value label 1 set to cat specific label
  # same for all 2
  # variable category 1 or 2 actual numeric value
  # variable cat label the representation of the number in var cat
  # variable set to variable
  strataCounter <- 1
  dimNames <- attr(passedTable$ContTable, "dimnames")
  #print(attributes(passedTable$ContTable))
  strataAllCombinationsDataFrame <- expand.grid(dimNames)
  strataArgs <- c(strataAllCombinationsDataFrame, sep = ":")
  strataValues <- do.call(paste, strataArgs)
  if (!is.null(passedTable$ContTable)) {
    ExtractDataFromContTable(
      passedTable$ContTable,
      attr(passedTable$ContTable, "strataVarName"),
      strataValues,
      variableDetails
    )
  }
}

ExtractDataFromContTable <-
  function(contTable,
           strataName,
           strataValues,
           variableDetails) {
    longTable <-
      data.frame(
        groupBy1 = "DummyRow",
        groupByValue1 = "DummyRow",
        groupByLabel1 = "DummyRow",
        groupByValueLabel1 = "DummyRow",
        groupBy2 = "DummyRow",
        groupByValue2 = "DummyRow",
        groupByLabel2 = "DummyRow",
        groupByValueLabel2 = "DummyRow",
        variableCategory = 1,
        variableCategoryLabel = "DummyRow",
        variable = "DummyRow",
        prevalence = 0.25,
        n = 1000,
        nMissing = 1000,
        mean = 100.100,
        sd = 100.100,
        percentile25 = 100.100,
        percentile75 = 100.100
      )
    
    strataSplitName <-
      unlist(strsplit(as.character(strataName), split = ":"))
    #print(strataSplitName)
    for (i in 1:length(contTable)) {
      variables <- (row.names(contTable[[i]]))
      for (row in 1:nrow(contTable[[i]])) {
        strataSplitValues <-
          unlist(strsplit(as.character(strataValues[[i]]), split = ":"))
        # extract all the information for that row
        num <- contTable[[i]][row, "n"]
        nMiss <- contTable[[i]][row, "miss"]
        rowMean <- contTable[[i]][row, "mean"]
        rowSD <- contTable[[i]][row, "sd"]
        rowPercentile25 <- contTable[[i]][row, "p25"]
        rowPercentile75 <- contTable[[i]][row, "p75"]
        # create the row to add to tableOne Long
        groupByLabel1 <-
          variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
                            compareNA(variableDetails$value, strataSplitValues[[1]]), "label"]
        groupByValueLabel1 <-
          variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
                            compareNA(variableDetails$value, strataSplitValues[[1]]), "value_label"]
        groupByLabel2 <-
          variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
                            compareNA(variableDetails$value, strataSplitValues[[2]])
                          , "label"]
        groupByValueLabel2 <-
          variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
                            compareNA(variableDetails$value, strataSplitValues[[2]])
                          , "value_label"]
        print(
          paste(
            variableDetails$variable,
            strataSplitName[[2]],
            variableDetails$value,
            strataSplitValues[[2]]
          )
        )
        print(groupByLabel2)
        print(groupByValueLabel2)
        longTableRow <- data.frame(
          groupBy1 = strataSplitName[[1]],
          groupByValue1 = strataSplitValues[[1]],
          groupByLabel1 = groupByLabel1,
          groupByValueLabel1 = groupByValueLabel1,
          groupBy2 = strataSplitName[[2]],
          groupByValue2 = strataSplitValues[[2]],
          groupByLabel2 = groupByLabel2,
          groupByValueLabel2 = groupByValueLabel2,
          variableCategory = NA,
          variableCategoryLabel = NA,
          variable = variables[[row]],
          prevalence = NA,
          n = num,
          nMissing = nMiss,
          mean = rowMean,
          sd = rowSD,
          percentile25 = rowPercentile25,
          percentile75 = rowPercentile75
        )
        longTable <- rbind(longTable, longTableRow)
      }
    }
    print(longTable)
  }

compareNA <- function(v1,v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
