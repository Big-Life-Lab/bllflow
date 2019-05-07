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
  # Create empty LongTable to append to
  longTable <-
    data.frame(
      groupBy1 = character(),
      groupByValue1 = numeric(),
      groupByLabel1 = character(),
      groupByValueLabel1 = character(),
      groupBy2 = character(),
      groupByValue2 = numeric(),
      groupByLabel2 = character(),
      groupByValueLabel2 = character(),
      variableCategory = character(),
      variableCategoryLabel = character(),
      variable = character(),
      prevalence = numeric(),
      n = numeric(),
      nMissing = numeric(),
      mean = numeric(),
      sd = numeric(),
      percentile25 = numeric(),
      percentile75 = numeric(),
      stringsAsFactors = FALSE
    )
  variablesForStrata <-
    unique(unlist(tableVariablesSheet[, grepl("pbcSummaryStat", colnames(tableVariablesSheet))]))
  variablesForStrata <- variablesForStrata[variablesForStrata != ""]
  tableOneTables <-
    lapply(variablesForStrata , function(strataValue)
      CreateCustomTableOne(strataValue, tableVariablesSheet, bllFlowModel))
  for (tableOne in tableOneTables) {
    longTable <-
      AddToLongTable(tableOne, longTable)
  }
  
  return(longTable)
}

# Create Table Ones from the tableVariablesSheet
CreateCustomTableOne <-
  function(strataValue, variableNames, bllFlowModel) {
    # Create vector of what variables to use
    variablesToUseBoolVector <-
      apply(variableNames, 1, function(row)
        any(row %in% c(as.character(strataValue))))
    tableOneVars <-
      variableNames$variables[variablesToUseBoolVector]
    # Seperate the Variables into continues and categorical
    categoricalVariables <- list()
    for (variable in tableOneVars) {
      variableType <-
        bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]][compareNA(bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]]$variable,
                                                                             variable) , "variableType"]
      if (variableType[[1]] == "category") {
        categoricalVariables <- c(categoricalVariables, variable)
      }
    }
    strataList <-
      unlist(strsplit(as.character(strataValue), split = ", "))
    retTable <-
      CreateTableOne(
        vars = as.character(tableOneVars),
        data = bllFlowModel$data,
        strata = strataList,
        factorVars = as.character(categoricalVariables)
      )
    
    return(retTable)
  }

# Function to create a long table one for one tableOne
AddToLongTable <-
  function(passedTable, longTable, variableDetails) {
    # Format Strata values into machine retable format
    dimNames <- attr(passedTable$ContTable, "dimnames")
    strataAllCombinationsDataFrame <- expand.grid(dimNames)
    strataArgs <- c(strataAllCombinationsDataFrame, sep = ":")
    strataValues <- do.call(paste, strataArgs)
    # Call Cont table extraction if tableOne contains ContTable
    if (!is.null(passedTable$ContTable)) {
      longTable <-
        ExtractDataFromContTable(
          passedTable$ContTable,
          attr(passedTable$ContTable, "strataVarName"),
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
          attr(passedTable$CatTable, "strataVarName"),
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
    # loop through each strata columns
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
        groupByList <- list()
        if (length(strataSplitName) > 0) {
        for (groupByIndex in 1:length(strataSplitName)) {
          tempReturn <-
            AddColumn(
              paste("groupBy", groupByIndex, sep = ""),
              strataSplitName[[groupByIndex]],
              groupByList,
              longTable
            )
          groupByList <- tempReturn[[1]]
          longTable <- tempReturn[[2]]
          
          tempReturn <-
            AddColumn(
              paste("groupByValue", groupByIndex, sep = ""),
              strataSplitValues[[groupByIndex]],
              groupByList,
              longTable
            )
          groupByList <- tempReturn[[1]]
          longTable <- tempReturn[[2]]
          
          if (!is.null(variableDetails)) {
            print(strataSplitValues[[groupByIndex]])
            print(as.character(variableDetails[compareNA(variableDetails$variable, strataSplitName[[groupByIndex]]) &
                                    compareNA(variableDetails$value, strataSplitValues[[groupByIndex]]), pkg.globals$argument.VariableStartLabel]))
            tempReturn <-
              AddColumn(
                paste("groupByLabel", groupByIndex, sep = ""),
                variableDetails[compareNA(variableDetails$variable, strataSplitName[[groupByIndex]]) &
                                  compareNA(variableDetails$value, strataSplitValues[[groupByIndex]]), pkg.globals$argument.VariableStartLabel],
                groupByList,
                longTable
              )
            groupByList <- tempReturn[[1]]
            longTable <- tempReturn[[2]]
            
            tempReturn <-
              AddColumn(
                paste("groupByValueLabel", groupByIndex, sep = ""),
                variableDetails[compareNA(variableDetails$variable, strataSplitName[[groupByIndex]]) &
                                  compareNA(variableDetails$value, strataSplitValues[[groupByIndex]]), pkg.globals$argument.CatStartLabel],
                groupByList,
                longTable
              )
            groupByList <- tempReturn[[1]]
            longTable <- tempReturn[[2]]
          }
        }
      }
        # groupByLabel1 <-
        #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
        #                     compareNA(variableDetails$value, strataSplitValues[[1]]), "label"]
        # groupByValueLabel1 <-
        #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
        #                     compareNA(variableDetails$value, strataSplitValues[[1]]), "valueLabel"]
        # groupByLabel2 <-
        #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
        #                     compareNA(variableDetails$value, strataSplitValues[[2]])
        #                   , "label"]
        # groupByValueLabel2 <-
        #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
        #                     compareNA(variableDetails$value, strataSplitValues[[2]])
        #                  , "valueLabel"]
        longTableRow <- list(
          variableCategory = NA,
          variable = variables[[row]],
          prevalence = NA,
          n = num,
          nMissing = nMiss,
          mean = rowMean,
          sd = rowSD,
          percentile25 = rowPercentile25,
          percentile75 = rowPercentile75
        )
        longTableRow <- append(longTableRow, groupByList)
        #print(longTableRow)
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
    for (strataCounter in 1:length(catTable)) {
      strataSplitValues <-
        unlist(strsplit(as.character(strataValues[[strataCounter]]), split = ":"))
      # Loop through the tables of each variable
      for (selectedVariable in catTable[[strataCounter]]) {
        variablesChecked <- variablesChecked + 1
        
        # Loop through the levels of each variable
        for (row in 1:nrow(selectedVariable)) {
          nMiss <- selectedVariable[row, "miss"]
          frequency <- selectedVariable[row, "freq"]
          levName <- selectedVariable[row, "level"]
          prevalence <- selectedVariable[row, "percent"]
          groupByList <- list()
          if (length(strataSplitName) > 0) {
          for (groupByIndex in 1:length(strataSplitName)) {
            tempReturn <-
              AddColumn(
                paste("groupBy", groupByIndex, sep = ""),
                strataSplitName[[groupByIndex]],
                groupByList,
                longTable
              )
            groupByList <- tempReturn[[1]]
            longTable <- tempReturn[[2]]
      
            tempReturn <-
              AddColumn(
                paste("groupByValue", groupByIndex, sep = ""),
                strataSplitValues[[groupByIndex]],
                groupByList,
                longTable
              )
            groupByList <- tempReturn[[1]]
            longTable <- tempReturn[[2]]
          }
        }
          # groupByLabel1 <-
          #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
          #                     compareNA(variableDetails$value, strataSplitValues[[1]]), "label"]
          # groupByValueLabel1 <-
          #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[1]]) &
          #                     compareNA(variableDetails$value, strataSplitValues[[1]]), "valueLabel"]
          # groupByLabel2 <-
          #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
          #                     compareNA(variableDetails$value, strataSplitValues[[2]])
          #                   , "label"]
          # groupByValueLabel2 <-
          #   variableDetails[compareNA(variableDetails$variable, strataSplitName[[2]]) &
          #                     compareNA(variableDetails$value, strataSplitValues[[2]])
          #                   , "valueLabel"]
          # variableCategoryLabel <-
          #   variableDetails[compareNA(variableDetails$variable, varNames[[variablesChecked]]) &
          #                     compareNA(variableDetails$value, as.character(levName))
          #                   , "valueLabel"]
          
          longTableRow <- list(
            variableCategory = levName,
            variable = varNames[variablesChecked],
            prevalence = prevalence,
            n = frequency,
            nMissing = nMiss,
            mean = NA,
            sd = NA,
            percentile25 = NA,
            percentile75 = NA
          )
          longTableRow <- append(longTableRow, groupByList)
          longTable <-
            rbind(longTable, longTableRow,  stringsAsFactors = FALSE)
        }
      }
      variablesChecked <- 0
    }
    
    return(longTable)
  }

# Function to compare even with NA present
compareNA <- function(v1, v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#'@export
SummaryDataLong <-
  function(tableOne,
           longTable = NULL,
           bllModel = NULL) {
    if (is.null(tableOne) & is.null(longTable)) {
      warning("No talbe one or long table was passed to SummaryDataLong",
              call. = FALSE)
    }
    if (is.null(longTable)) {
      longTable <- data.frame(
        variableCategory = character(),
        variable = character(),
        prevalence = numeric(),
        n = numeric(),
        nMissing = numeric(),
        mean = numeric(),
        sd = numeric(),
        percentile25 = numeric(),
        percentile75 = numeric(),
        stringsAsFactors = FALSE
      )
    }
    AddToLongTable(tableOne, longTable, bllModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]])
  }

#' @export
CreateTableOne <- function(x = NULL, ...) {
  UseMethod("CreateTableOne", x)
}

#' If no vars are present use the variables inside bllVariables
#' @export
CreateTableOne.BLLFlow <- function(bllFlowModel,
                                   vars = NULL,
                                   strata = NULL,
                                   factorVars = NULL) {
  #pull from variables in bllFlow
  #   variableDetails[compareNA(variableDetails$variable, varNames[[variablesChecked]]) &
  #                     compareNA(variableDetails$value, as.character(levName))
  #                   , "valueLabel"]
  variablesSheet <- bllFlowModel[[pkg.globals$bllFlowContent.Variables]]
  if (is.null(vars)) {
    vars <-
      as.character(bllFlowModel[[pkg.globals$bllFlowContent.Variables]][, pkg.globals$MSW.Variables.Columns.Variable])
  }
  if (is.null(factorVars)) {
    factorVars <-
      as.character(variablesSheet[compareNA(variablesSheet[[pkg.globals$MSW.Variables.Columns.VariableType]], pkg.globals$ddiValueName.Categorical) , pkg.globals$MSW.Variables.Columns.Variable])
  }
  print(vars)
  if (is.null(strata)) {
    finalTable <-
      tableone::CreateTableOne(data = bllFlowModel[[pkg.globals$bllFlowContent.Data]], vars = vars, factorVars = factorVars)
  }else{
  finalTable <-
    tableone::CreateTableOne(data = bllFlowModel[[pkg.globals$bllFlowContent.Data]], vars = vars, factorVars = factorVars, strata = strata)
  }
  
  return(finalTable)
}

#' @export
CreateTableOne.default <- tableone::CreateTableOne

AddColumn <-
  function(columnName,
           valueToSet,
           listToAddTo,
           tableToAddTo) {
    listToAddTo[[columnName]] <- valueToSet
    if (!columnName %in% colnames(tableToAddTo)) {
      if (nrow(tableToAddTo) == 0) {
        tableToAddTo[[columnName]] <- character()
      } else {
        tableToAddTo[[columnName]] <- NA
      }
    }
    return(list(listToAddTo, tableToAddTo))
  }
