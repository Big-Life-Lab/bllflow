#' Summary Data Long Table
#'
#' Creates a Long table to summarise data from multiple tables in one convenient table.
#' Its primary use is to convert Table one tables into a long table.
#' The optional arguments allow appending to long table as well as addition of labels
#'
#' @param tableOne the table one object to be converted into a long table
#' @param longTable the optional long table to append the table one information to
#' @param bllFlowModel The optional bllFlow object containing labels and extra information on the variables
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
      warning("No table one or long table was passed to SummaryDataLong",
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
    } else{
      longTable <- longTable[[pkg.globals$LongTable.LongTable]]
    }
    returnTable <-
      AddToLongTable(tableOne, longTable, bllFlowModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]])
    if (!pkg.globals$LongTable.ClassName %in% class(returnTable)) {
      class(returnTable) <-
        append(class(returnTable), pkg.globals$LongTable.ClassName)
    }
    returnTable <- unique(returnTable)
    returnSummaryData <- list(summaryData = returnTable)
    class(returnSummaryData) <- "SummaryData"
    
    return(returnSummaryData)
    
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
  # ----Step 1: pull from variables in bllFlowModel ----
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
  
  # ----Step 2: Create the tableone ----
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
    # ----Step 1: Populate long table from cont and cat tableone tables ----
    # Call Cont table extraction if tableOne contains ContTable
    returnedLongTables <- list()
    tableCount <- 0
    if (!is.null(passedTable$ContTable)) {
      dimNames <- attr(passedTable$ContTable, "dimnames")
      strataValues <- CleanStrataValues(dimNames)
      tableCount <- tableCount + 1
      contTableLongTable <-
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
      returnedLongTables[[tableCount]] <- contTableLongTable
    }
    
    # Call Cat table extraction if tableOne contains CatTable
    if (!is.null(passedTable$CatTable)) {
      dimNames <- attr(passedTable$CatTable, "dimnames")
      strataValues <- CleanStrataValues(dimNames)
      tableCount <- tableCount + 1
      catTableLongTable <-
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
      returnedLongTables[[tableCount]] <- catTableLongTable
    }
    
    # ----Step 2: Add any missing columns to the newly created tables----
    for (tableToAppend in returnedLongTables) {
      for (columnMissing in colnames(longTable)) {
        if (!columnMissing %in% colnames(tableToAppend)) {
          tableToAppend[[columnMissing]] <- NA
        }
      }
      for (columnMissing in colnames(tableToAppend)) {
        if (!columnMissing %in% colnames(longTable)) {
          # in case of zero row table columns need to be declared in columns <- dataType()
          # Set data type of missing column to type of append table
          if (nrow(longTable) == 0) {
            class(longTable[[columnMissing]]) <-
              class(tableToAppend[[columnMissing]])
          } else {
            longTable[[columnMissing]] <- NA
          }
        }
      }
      
      longTable <-
        rbind(longTable, tableToAppend,  stringsAsFactors = FALSE)
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
    
    # ----Step 1: Split the strata name into the two variables ----
    if (!is.null(strataName)) {
      strataSplitName <-
        unlist(strsplit(as.character(strataName), split = ":"))
    } else{
      strataSplitName <- strataName
    }
    
    # ----Step 2: Add columns to long table
    longTableRows <- data.frame()
    
    # loop through each strata columns
    # ----Step 3: Extract information for each new row of the longtable ----
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
        if (length(strataSplitName) > 0) {
          groupByList <-
            FillInGroupByColumns(strataSplitName,
                                 strataSplitValues,
                                 groupByList,
                                 variableDetails)
        }
        
        # ----Step 4: Create long table row ----
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
        
        # ----Step 5: Clean the row
        for (eachElementIndex in 1:length(longTableRow)) {
          # remove empty classes to avoid bind conflicts
          # example character(0)
          if (length(longTableRow[[eachElementIndex]]) == 0) {
            longTableRow[[eachElementIndex]] <- NA
          }
        }
        
        # ----Step 6: Add row to the rest of the rows----
        longTableRows <-
          rbind(longTableRows, longTableRow,  stringsAsFactors = FALSE)
      }
    }
    
    return(longTableRows)
  }

# Create long table from CatTable
ExtractDataFromCatTable <-
  function(catTable,
           strataName,
           strataValues,
           longTable,
           variableDetails) {
    # ----Step 1: Split the strata name into the two variables ----
    variablesChecked <- 0
    varNames <- attr(catTable[[1]], "names")
    strataSplitName <-
      unlist(strsplit(as.character(strataName), split = ":"))
    # Adds group by columns not found in the long table
    
    # ----Step 2: Add columns to long table
    longTableRows <- data.frame()
    
    # ----Step 3: Extract information for each new row of the longtable ----
    for (strataCounter in 1:length(catTable)) {
      strataSplitValues <-
        unlist(strsplit(as.character(strataValues[[strataCounter]]), split = ":"))
      # Loop through the tables of each variable
      for (selectedVariableTable in catTable[[strataCounter]]) {
        variablesChecked <- variablesChecked + 1
        
        # Loop through the levels of each variable
        for (row in 1:nrow(selectedVariableTable)) {
          nMiss <- selectedVariableTable[row, pkg.globals$tableOne.Miss]
          frequency <-
            selectedVariableTable[row, pkg.globals$tableOne.Freq]
          levName <-
            selectedVariableTable[row, pkg.globals$tableOne.Level]
          prevalence <-
            selectedVariableTable[row, pkg.globals$tableOne.Percent]
          groupByList <- list()
          if (length(strataSplitName) > 0) {
            groupByList <-
              FillInGroupByColumns(strataSplitName,
                                   strataSplitValues,
                                   groupByList,
                                   variableDetails)
            if (!is.null(variableDetails)) {
              groupByList[[pkg.globals$LongTable.VariableCategoryLabel]] <-
                variableDetails[isEqual(variableDetails[[pkg.globals$argument.VariableStart]], varNames[[variablesChecked]]) &
                                  isEqual(variableDetails[[pkg.globals$argument.CatStartValue]], as.character(levName)), pkg.globals$argument.CatStartLabel]
              # If empty add NA
              if (length(groupByList[[pkg.globals$LongTable.VariableCategoryLabel]]) == 0) {
                groupByList[[pkg.globals$LongTable.VariableCategoryLabel]] <- NA
              }
            }
          }
          
          # ----Step 4: Create long table row ----
          longTableRow <- list()
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
          
          # ----Step 5: Clean the row
          for (eachElementIndex in 1:length(longTableRow)) {
            if (length(longTableRow[[eachElementIndex]]) == 0) {
              longTableRow[[eachElementIndex]] <- NA
            }
          }
          
          # ----Step 6: Add row to the rest of the rows----
          longTableRows <-
            rbind(longTableRows, longTableRow,  stringsAsFactors = FALSE)
        }
      }
      variablesChecked <- 0
    }
    
    return(longTableRows)
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