#' Summary Data Long Table
#'
#' Creates a Long table to summarise data from multiple tables in one convinient table.
#' Its primary use is to convert Table one tables into long table.
#' The optional arguments allow appending to long table as well as addition of labeles
#'
#' @param tableOne the table one object to be converted into a long table
#' @param longTable the optional long table to append the table one information to
#' @param bllFlowModel The optional bllObject containing labels and extra information on the variables
#' @return Returns the long table or the bllFlowModel with long table attached
#'
#' @examples
#' library(survival)
#' data(pbc)
#' pbc$exp_percentile <- runif(nrow(pbc), 0, 1)
#' pbc$ageGroup <- ifelse(pbc$age < 20, 1,
#' ifelse(pbc$age >= 20 & pbc$age < 40, 2,
#' ifelse(pbc$age >= 40 & pbc$age < 80, 3,
#' ifelse(pbc$age >= 80, 4, NA))))
#'
#' library(bllflow)
#' variablesSheet <- read.csv(file.path(getwd(), '../inst/extdata/PBC-variables.csv'))
#' variablesDetailsSheet <- read.csv(file.path(getwd(), '../inst/extdata/PBC-variableDetails.csv'))
#' ddi <- ReadDDI(file.path(getwd(), '../inst/extdata'),"pbcDDI.xml")
#' pbcModel <- BLLFlow(pbc, variablesSheet, variablesDetailsSheet, ddi)
#'
#' pbcTableOne <- CreateTableOne(pbcModel, strata = "edema")
#' pbcSummaryTableNoLabels <- SummaryDataLong(pbcTableOne)
#' pbcLongTableWithLabel <- SummaryDataLong(pbcTableOne, bllFlowModel = pbcModel, longTable = pbcSummaryTableNoLabels)
#'@export
SummaryDataLong <-
  function(tableOne,
           longTable = NULL,
           bllFlowModel = NULL) {
    if (is.null(tableOne) & is.null(longTable)) {
      warning("No talbe one or long table was passed to SummaryDataLong",
              call. = FALSE)
    }
    if (is.null(longTable)) {
      longTable <- data.frame(stringsAsFactors = FALSE)
      longTable[[pkg.globals$LongTable.VariableCategory]] <-
        character()
      longTable[[pkg.globals$LongTable.Variable]] <- character()
      longTable[[pkg.globals$LongTable.Prevalence]] <-  numeric()
      longTable[[pkg.globals$LongTable.Frequency]] <-  numeric()
      longTable[[pkg.globals$LongTable.NMissing]] <-  numeric()
      longTable[[pkg.globals$LongTable.Mean]] <-  numeric()
      longTable[[pkg.globals$LongTable.SD]] <-  numeric()
      longTable[[pkg.globals$LongTable.Percentile25]] <-  numeric()
      longTable[[pkg.globals$LongTable.Percentile75]] <-  numeric()
    }
    returnTable <-
      AddToLongTable(tableOne, longTable, bllFlowModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]])
    returnTable <- unique(returnTable)
    retObject <- NULL
    if (is.null(bllFlowModel)) {
      retObject <- returnTable
    } else {
      bllFlowModel$longTable <- returnTable
      retObject <- bllFlowModel
    }
    
    return(retObject)
  }

#' Create Table One
#'
#' Creates Table One using the tableone package
#' @export
CreateTableOne <- function(x = NULL, ...) {
  UseMethod("CreateTableOne", x)
}

#' Create Table One using BLLFlow Object
#'
#' Creates table one using the information present in the passed bllFlow object
#' additional arguments can be passed to create a specific table one.
#' However if no optional args are passed the variable info stored in variables MSW is used.
#'
#' @param bllFlowModel The bllflow object
#' @param vars The optional vars to use in creation of table one if no vars are passed then vars in MSW variables is used
#' @param strata The optional strata to use in creation of table one if no strata is passed no strata is used
#' @param factorVars The optional factorVars (categorical variables) used in creation of table one if nothing is passed
#' the MSW variables sheet is used to determine variable types
#'
#' @return returns a table one tableOne object
#'
#' @examples
#' library(survival)
#' data(pbc)
#' pbc$exp_percentile <- runif(nrow(pbc), 0, 1)
#' pbc$ageGroup <- ifelse(pbc$age < 20, 1,
#' ifelse(pbc$age >= 20 & pbc$age < 40, 2,
#' ifelse(pbc$age >= 40 & pbc$age < 80, 3,
#' ifelse(pbc$age >= 80, 4, NA))))
#'
#' library(bllflow)
#' variablesSheet <- read.csv(file.path(getwd(), '../inst/extdata/PBC-variables.csv'))
#' variablesDetailsSheet <- read.csv(file.path(getwd(), '../inst/extdata/PBC-variableDetails.csv'))
#' ddi <- ReadDDI(file.path(getwd(), '../inst/extdata'),"pbcDDI.xml")
#' pbcModel <- BLLFlow(pbc, variablesSheet, variablesDetailsSheet, ddi)
#'
#' pbcTableOne <- CreateTableOne(pbcModel, strata = "edema")
#'
#' @export
CreateTableOne.BLLFlow <- function(bllFlowModel,
                                   vars = NULL,
                                   strata = NULL,
                                   factorVars = NULL) {
  #pull from variables in bllFlowModel
  variablesSheet <-
    bllFlowModel[[pkg.globals$bllFlowContent.Variables]]
  if (is.null(vars)) {
    vars <-
      as.character(bllFlowModel[[pkg.globals$bllFlowContent.Variables]][, pkg.globals$MSW.Variables.Columns.Variable])
  }
  if (is.null(factorVars)) {
    factorVars <-
      as.character(variablesSheet[isEqual(variablesSheet[[pkg.globals$MSW.Variables.Columns.VariableType]], pkg.globals$ddiValueName.Categorical) , pkg.globals$MSW.Variables.Columns.Variable])
  }
  
  if (is.null(strata)) {
    finalTable <-
      tableone::CreateTableOne(data = bllFlowModel[[pkg.globals$bllFlowContent.Data]],
                               vars = vars,
                               factorVars = factorVars)
  } else{
    finalTable <-
      tableone::CreateTableOne(
        data = bllFlowModel[[pkg.globals$bllFlowContent.Data]],
        vars = vars,
        factorVars = factorVars,
        strata = strata
      )
  }
  
  return(finalTable)
}

#' @export
CreateTableOne.default <- tableone::CreateTableOne

# Function to create a long table one for one tableOne
AddToLongTable <-
  function(passedTable, longTable, variableDetails) {
    # Format Strata values into machine readable format
    dimNames <- attr(passedTable$ContTable, "dimnames")
    strataAllCombinationsDataFrame <- expand.grid(dimNames)
    strataArgs <- c(strataAllCombinationsDataFrame, sep = ":")
    strataValues <- do.call(paste, strataArgs)
    
    # Call Cont table extraction if tableOne contains ContTable
    if (!is.null(passedTable$ContTable)) {
      longTable <-
        ExtractDataFromContTable(
          passedTable$ContTable,
          attr(
            passedTable$ContTable,
            pkg.globals$tableOne.StrataVarName
          ),
          strataValues,
          longTable,
          variableDetails
        )
    }
    
    # Call Cat table extraction if tableOne contains CatTable
    if (!is.null(passedTable$CatTable)) {
      longTable <-
        ExtractDataFromCatTable(
          passedTable$CatTable,
          attr(
            passedTable$CatTable,
            pkg.globals$tableOne.StrataVarName
          ),
          strataValues,
          longTable,
          variableDetails
        )
    }
    
    return(longTable)
  }

# Create long table from contTable
ExtractDataFromContTable <-
  function(contTable,
           strataName,
           strataValues,
           longTable,
           variableDetails) {
    strataSplitName <- character()
    # Split the strata name into he two variables
    if (!is.null(strataName)) {
      strataSplitName <-
        unlist(strsplit(as.character(strataName), split = ":"))
    } else{
      strataSplitName <- strataName
    }
    longTable <-
      AddGroupByColumns(strataSplitName, longTable, variableDetails)
    # loop through each strata columns
    for (strataIndex in 1:length(contTable)) {
      variables <- (row.names(contTable[[strataIndex]]))
      for (row in 1:nrow(contTable[[strataIndex]])) {
        strataSplitValues <-
          unlist(strsplit(as.character(strataValues[[strataIndex]]), split = ":"))
        # extract all the information for that row
        num <- contTable[[strataIndex]][row, pkg.globals$tableOne.N]
        nMiss <-
          contTable[[strataIndex]][row, pkg.globals$tableOne.Miss]
        rowMean <-
          contTable[[strataIndex]][row, pkg.globals$tableOne.Mean]
        rowSD <-
          contTable[[strataIndex]][row, pkg.globals$tableOne.SD]
        rowPercentile25 <-
          contTable[[strataIndex]][row, pkg.globals$tableOne.p25]
        rowPercentile75 <-
          contTable[[strataIndex]][row, pkg.globals$tableOne.p75]
        
        # create the row to add to tableOne Long
        groupByList <- list()
        longTableRow <- list()
        if (length(strataSplitName) > 0) {
          longTableRow <-
            FillInGroupByColumns(strataSplitName,
                                 strataSplitValues,
                                 longTableRow,
                                 variableDetails)
        }
        longTableRow <- list()
        longTableRow[[pkg.globals$LongTable.VariableCategory]] <- NA
        longTableRow[[pkg.globals$LongTable.Variable]] <-
          variables[[row]]
        longTableRow[[pkg.globals$LongTable.Prevalence]] <-  NA
        longTableRow[[pkg.globals$LongTable.Frequency]] <-  num
        longTableRow[[pkg.globals$LongTable.NMissing]] <-  nMiss
        longTableRow[[pkg.globals$LongTable.Mean]] <-  rowMean
        longTableRow[[pkg.globals$LongTable.SD]] <-  rowSD
        longTableRow[[pkg.globals$LongTable.Percentile25]] <-
          rowPercentile25
        longTableRow[[pkg.globals$LongTable.Percentile75]] <-
          rowPercentile75
        
        longTableRow <- append(longTableRow, groupByList)
        for (columnMissing in colnames(longTable)) {
          if (!columnMissing %in% names(longTableRow)) {
            longTableRow[[columnMissing]] <- NA
          }
        }
        longTable <-
          rbind(longTable, longTableRow,  stringsAsFactors = FALSE)
      }
    }
    
    return(longTable)
  }

# Create long table from CatTable
ExtractDataFromCatTable <-
  function(catTable,
           strataName,
           strataValues,
           longTable,
           variableDetails) {
    variablesChecked <- 0
    varNames <- attr(catTable[[1]], "names")
    strataSplitName <-
      unlist(strsplit(as.character(strataName), split = ":"))
    # Adds group by columns not found in the long table
    longTable <-
      AddGroupByColumns(strataSplitName, longTable, variableDetails)
    for (strataCounter in 1:length(catTable)) {
      strataSplitValues <-
        unlist(strsplit(as.character(strataValues[[strataCounter]]), split = ":"))
      # Loop through the tables of each variable
      for (selectedVariable in catTable[[strataCounter]]) {
        variablesChecked <- variablesChecked + 1
        
        # Loop through the levels of each variable
        for (row in 1:nrow(selectedVariable)) {
          nMiss <- selectedVariable[row, pkg.globals$tableOne.Miss]
          frequency <-
            selectedVariable[row, pkg.globals$tableOne.Freq]
          levName <-
            selectedVariable[row, pkg.globals$tableOne.Level]
          prevalence <-
            selectedVariable[row, pkg.globals$tableOne.Percent]
          groupByList <- list()
          longTableRow <- list()
          if (length(strataSplitName) > 0) {
            longTableRow <-
              FillInGroupByColumns(strataSplitName,
                                   strataSplitValues,
                                   longTableRow,
                                   variableDetails)
            if (!is.null(variableDetails)) {
              longTableRow[["variableCategoryLabel"]] <-
                variableDetails[isEqual(variableDetails[[pkg.globals$argument.VariableStart]], varNames[[variablesChecked]]) &
                                  isEqual(variableDetails[[pkg.globals$argument.CatStartValue]], as.character(levName)), pkg.globals$argument.CatStartLabel]
              longTable <- AddColumn("variableCategoryLabel",
                                     longTable)
            }
          }
          longTableRow[[pkg.globals$LongTable.VariableCategory]] <-
            levName
          longTableRow[[pkg.globals$LongTable.Variable]] <-
            varNames[variablesChecked]
          longTableRow[[pkg.globals$LongTable.Prevalence]] <-
            prevalence
          longTableRow[[pkg.globals$LongTable.Frequency]] <-
            frequency
          longTableRow[[pkg.globals$LongTable.NMissing]] <-  nMiss
          longTableRow[[pkg.globals$LongTable.Mean]] <-  NA
          longTableRow[[pkg.globals$LongTable.SD]] <-  NA
          longTableRow[[pkg.globals$LongTable.Percentile25]] <-  NA
          longTableRow[[pkg.globals$LongTable.Percentile75]] <-  NA
          
          longTableRow <- append(longTableRow, groupByList)
          for (columnMissing in colnames(longTable)) {
            if (!columnMissing %in% names(longTableRow)) {
              longTableRow[[columnMissing]] <- NA
            }
          }
          longTable <-
            rbind(longTable, longTableRow,  stringsAsFactors = FALSE)
        }
      }
      variablesChecked <- 0
    }
    
    return(longTable)
  }
# deprecated ----------------------------------------------------------------------
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
#'
#' @return A dataframe containing the table one long
#'
#' @import tableone
# tables.CreateTableOneLong <- function(bllFlowModel,
#                                       tableVariablesSheet) {
#   # Create empty LongTable to append to
#   longTable <-
#     data.frame(
#       groupBy1 = character(),
#       groupByValue1 = numeric(),
#       groupByLabel1 = character(),
#       groupByValueLabel1 = character(),
#       groupBy2 = character(),
#       groupByValue2 = numeric(),
#       groupByLabel2 = character(),
#       groupByValueLabel2 = character(),
#       variableCategory = character(),
#       variableCategoryLabel = character(),
#       variable = character(),
#       prevalence = numeric(),
#       n = numeric(),
#       nMissing = numeric(),
#       mean = numeric(),
#       sd = numeric(),
#       percentile25 = numeric(),
#       percentile75 = numeric(),
#       stringsAsFactors = FALSE
#     )
#   variablesForStrata <-
#     unique(unlist(tableVariablesSheet[, grepl("pbcSummaryStat", colnames(tableVariablesSheet))]))
#   variablesForStrata <- variablesForStrata[variablesForStrata != ""]
#   tableOneTables <-
#     lapply(variablesForStrata , function(strataValue)
#       CreateCustomTableOne(strataValue, tableVariablesSheet, bllFlowModel))
#   for (tableOne in tableOneTables) {
#     longTable <-
#       AddToLongTable(tableOne, longTable)
#   }
#
#   return(longTable)
# }
#
# # Create Table Ones from the tableVariablesSheet
# CreateCustomTableOne <-
#   function(strataValue, variableNames, bllFlowModel) {
#     # Create vector of what variables to use
#     variablesToUseBoolVector <-
#       apply(variableNames, 1, function(row)
#         any(row %in% c(as.character(strataValue))))
#     tableOneVars <-
#       variableNames$variables[variablesToUseBoolVector]
#     # Seperate the Variables into continues and categorical
#     categoricalVariables <- list()
#     for (variable in tableOneVars) {
#       variableType <-
#         bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]][isEqual(bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]]$variable,
#                                                                            variable) , "variableType"]
#       if (variableType[[1]] == "category") {
#         categoricalVariables <- c(categoricalVariables, variable)
#       }
#     }
#     strataList <-
#       unlist(strsplit(as.character(strataValue), split = ", "))
#     retTable <-
#       CreateTableOne(
#         vars = as.character(tableOneVars),
#         data = bllFlowModel$data,
#         strata = strataList,
#         factorVars = as.character(categoricalVariables)
#       )
#
#     return(retTable)
#   }
