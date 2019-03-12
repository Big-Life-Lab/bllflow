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
tables.CreateTableOneLong <- function(
  bllFlowModel,
  tableVariablesSheet
) {
  # Extract the needed info from tableVariablesSheet to create the tableOne tables
  variablesForStrata <- unique(unlist(tableVariablesSheet[,grepl("pbcSummaryStat",colnames(tableVariablesSheet))]))
  variablesForStrata <- variablesForStrata[variablesForStrata!=""]
  tablesTB <- lapply(variablesForStrata , function(strataValue) CreateCustomTableOne(strataValue, tableVariablesSheet,bllFlowModel) )
  #print(attributes(tablesTB[[1]]))
  AddToLongTable(tablesTB[[1]])
  return(tablesTB)
  
}

CreateCustomTableOne <- function(strataValue, variableNames,bllFlowModel){
  test <- apply(variableNames, 1, function(row) any(row %in% c(as.character(strataValue))))
  tableOneVars <- variableNames$variables[test]
  strataList <- unlist(strsplit(as.character(strataValue), split = ", "))
  retTable <- CreateTableOne(vars = as.character(tableOneVars),data = bllFlowModel$data,strata = strataList)
  #print(attributes(retTable))
  return(retTable)
}

AddToLongTable <- function(passedTable){
  # group_by_1 set to first strat
  # group by value 1 set to first strata value
  # group by label 1 set to label of the variable
  # group by value label 1 set to cat specific label
  # same for all 2
  # variable category 1 or 2 actual numeric value
  # variable cat label the representation of the number in var cat
  # variable set to variable
  strataCounter <- 1
  print(passedTable$ContTable[1])
 
}

