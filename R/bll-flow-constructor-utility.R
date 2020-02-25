#' Initialize bllflow from provided config file
#' 
#' Used the provided config file and matching name to load the correct config 
#' type
#' 
#' @param config_name = NULL name of the config environment to use for
#' initialization
#' 
#' @return constructed bllflow object
#' @export
bllflow_config_init <-
  function(config_name = NULL) {
    # Idea let them pass config or config name? @Doug, @Warsame
    if (!is.null(config_name)) {
      Sys.setenv(R_CONFIG_ACTIVE = config_name)
    }
    config <- config::get()
    ret_bllflow <-
      build_bllflow(variables = as.data.frame(config$variables),
                    variable_details = as.data.frame(config$variable_details))
    
    return(ret_bllflow)
}

#' Read in data according to config specified data type
#' 
#' Uses bllflow to read_data based on config specifications. Currently supported
#' formats are: .RData, .csv
#' 
#' @param bllflow_object passed bllflow object to read variables from
#' @param config_name = NULL optional passing of config if you wish to load data
#' from a specific config
#' 
#' @return NULL since no modifications are made and read data is just stored in 
#' pre specified location
#' @export
bllflow_config_read_data <- function(bllflow_object, config_name = NULL){
  if (!is.null(config_name)){
    Sys.setenv(R_CONFIG_ACTIVE = config_name)
  }
  config <- config::get()
  if (config$data_type == ".RData") {
    # use variables to only read the specified variables??
  } else if (config$data_type == ".csv") {
    for (data_name in names(config$data)) {
      tmp_data <-
        read_data(
          variables = bllflow_object$variables,
          data_name = data_name,
          path_to_data = config$data[[data_name]]
        )
      assign(data_name, tmp_data)
      save(list = data_name, file = paste0(config$data_dir, data_name, ".RData"))
    }
  }
}

#' Recode data using config data
#' 
#' Recodes data according to the config then saves it as RData file at a 
#' specified location
#' 
#' @param bllflow_object passed bllflow object to read variables from
#' @param config_name = NULL optional passing of config if you wish to load data
#' from a specific config
#' 
#' @return NULL since no modifications are made and read data is just stored in 
#' pre specified location
#' @export
bllflow_config_rec_data <- function(bllflow_object, config_name = NULL){
  # Consider making this into a function or let user pass loaded config
  if (!is.null(config_name)){
    Sys.setenv(R_CONFIG_ACTIVE = config_name)
  }
  config <- config::get()
  for (data_name in names(config$data)) {
    load(file.path(config$data_dir, paste0( data_name, ".RData")))
    tmp_rec_data <- rec_with_table(base::get(data_name), variables = bllflow_object$variables)
    assign(data_name, tmp_rec_data)
    save(list = data_name, file = file.path(config$data_dir, paste0(data_name, "_recoded", ".RData")))
  }
}
# ----------- WIP NOT FULLY IMPLEMENTED ON TODO ---------
#' #' @export
#' create_variable_details_template <- function(x = NULL, ...) {
#'   UseMethod("create_variable_details_template", x)
#' }
#' 
#' #' @export
#' create_variable_details_template.BLLFlow <- function(bllFlow_object) {
#'   variable_details <-
#'     data.frame(
#'       variable = character(),
#'       toType = character(),
#'       databaseStart = character(),
#'       variableStart = character(),
#'       fromType = character(),
#'       recTo = character(),
#'       catLabel = character(),
#'       catLabelLong = character(),
#'       units = character(),
#'       recFrom = character(),
#'       catStartLabel = character(),
#'       variableStartShortLabel = character(),
#'       variableStartLabel = character()
#'     )
#'   # Collect all the variables in MSW variables
#'   detected_variables <- unique(bllFlow_object[[pkg.globals$bllFlowContent.Variables]][[pkg.globals$argument.Variables]])
#'   
#'   # Loop through the ddiList and add variables detected
#'   for (single_DDI in bllFlow_object[[pkg.globals$bllFlowContent.DDI]]) {
#'     variable <- "Please Insert RecodedVariable name"
#'     toType <-
#'       "Please insert desired recoded variable type supported ones are: cat, cont"
#'     databaseStart <-
#'       single_DDI[["ddiObject"]][["codeBook"]][["docDscr"]][["docSrc"]][["titlStmt"]][["titl"]][[1]]
#'     # loop through detected_variables
#'     for (singleDetectedVariable in detected_variables) {
#'       if (singleDetectedVariable %in% names(single_DDI[["variableMetaData"]][["dataDscr"]])) {
#'         variableDDI <-
#'           single_DDI[["variableMetaData"]][["dataDscr"]][[singleDetectedVariable]]
#'         variableStart <-
#'           paste(databaseStart, singleDetectedVariable, sep = "::")
#'         fromType <- variableDDI$type
#'         recTo <- "Please insert values to recode to"
#'         catLabel <- "Please enter the lable"
#'         catLabelLong <- "Please enter the long label"
#'         units <- "Specify the units"
#'         recFrom <- "Specify range to recode from"
#'         catStartLabel <- variableDDI$label
#'         variableStartShortLabel <- variableDDI$label
#'         variableStartLabel <- variableDDI$label
#'         
#'         newRow <-
#'           data.frame(
#'             variable = variable,
#'             toType,
#'             databaseStart,
#'             variableStart,
#'             fromType,
#'             recTo,
#'             catLabel,
#'             catLabelLong,
#'             units,
#'             recFrom,
#'             catStartLabel,
#'             variableStartShortLabel ,
#'             variableStartLabel
#'           )
#'         variable_details <- rbind(variable_details, newRow)
#'       }
#'     }
#'     
#'     
#'   }
#'   bllFlow_object$variable_details <- variable_details
#'   
#'   return(bllFlow_object)
#' }

# ----------- DEPRICATE NEEDS REMAKING ---------
#' #' Creates a data frame that holds additional ddi data
#' #'
#' #' @param variable_details The dataframe that contains the variable information
#' #' that is used to populate the frame with relevant ddi info
#' #' @param ddiVariables an object that is generated by populateVariables
#' #' it contains the variables as well as all their value labels and min and max
#' #' @return returns a dataframe containing new ddi data
#' PopulateVariableDetails <-
#'   function(variable_details,
#'            ddiVariables) {
#'     # Used to group all the variables in the dataframe
#'     variable_details <-
#'       variable_details[order(variable_details[[pkg.globals$argument.VariableStart]],
#'                                 variable_details[[pkg.globals$argument.CatStartValue]]),]
#'     onlyDesiredVariables <-
#'       variable_details[variable_details[[pkg.globals$argument.VariableStart]] %in% names(ddiVariables), ]
#'     # Copy all the columns
#'     finalFrame <- onlyDesiredVariables[0, ]
#'     for (nameIndex in 1:length(names(ddiVariables))) {
#'       nameBeingChecked <- names(ddiVariables)[[nameIndex]]
#'       # All the rows for the variable being checked
#'       rowsToCheck <-
#'         onlyDesiredVariables[onlyDesiredVariables[[pkg.globals$argument.VariableStart]] == nameBeingChecked,]
#'       # Writes data to relavant rows and removes them from the value object
#'       for (rowToCheck in 1:nrow(rowsToCheck)) {
#'         presentCatStartValue <-
#'           rowsToCheck[rowToCheck, pkg.globals$argument.CatStartValue]
#'         # Check if the value matches anything in the DDI object
#'         if (presentCatStartValue %in% names(ddiVariables[[nameBeingChecked]])) {
#'           # Populate every column with values pulled from DDI
#'           selectedVariableCatValue <-
#'             ddiVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]]
#'           for (columnName in names(selectedVariableCatValue)) {
#'             if (columnName != pkg.globals$argument.CatStartValue) {
#'               # Check if there is any data precent in the cell in order to not override anything
#'               if (CheckIfCellIsEmpty(
#'                 rowsToCheck[rowToCheck, columnName],
#'                 rownames(rowsToCheck)[rowToCheck],
#'                 columnName,
#'                 selectedVariableCatValue[[columnName]]
#'               )) {
#'                 # If this has not been in the dataframe upon creation that level is added
#'                 if (!selectedVariableCatValue[[columnName]] %in% levels(rowsToCheck[, columnName])) {
#'                   levels(rowsToCheck[, columnName]) <-
#'                     c(levels(rowsToCheck[, columnName]),
#'                       selectedVariableCatValue[[columnName]])
#'                 }
#'                 rowsToCheck[rowToCheck, columnName] <-
#'                   selectedVariableCatValue[[columnName]]
#'               }
#'             }
#'           }
#'           
#'           # Remove that value from the list to avoid repetition during new row creation
#'           ddiVariables[[nameBeingChecked]][[as.character(presentCatStartValue)]] <-
#'             NULL
#'           finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck, ])
#'         } else if (!is.null(ddiVariables[[nameBeingChecked]][[nameBeingChecked]]) &
#'                    !is.null(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartType]) &
#'                    !is.null(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartHigh]) &
#'                    !is.null(rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartLow])) {
#'           contVariableBeingChecked <-
#'             ddiVariables[[nameBeingChecked]][[nameBeingChecked]]
#'           if (rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartHigh] == contVariableBeingChecked[[pkg.globals$argument.VariableStartHigh]] &
#'               rowsToCheck[rowToCheck, pkg.globals$argument.VariableStartLow] == contVariableBeingChecked[[pkg.globals$argument.VariableStartLow]]) {
#'             # Populate every column with values pulled from DDI
#'             for (columnName in names(ddiVariables[[nameBeingChecked]][[as.character(nameBeingChecked)]])) {
#'               # Check if there is any data precent in the cell in order to not override anything
#'               if (CheckIfCellIsEmpty(rowsToCheck[rowToCheck, columnName],
#'                                      rownames(rowsToCheck)[rowToCheck],
#'                                      columnName,
#'                                      ddiVariables[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]])) {
#'                 # If this has not been in the dataframe upon creation that level is added
#'                 if (!ddiVariables[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]] %in% levels(rowsToCheck[, columnName])) {
#'                   levels(rowsToCheck[, columnName]) <-
#'                     c(levels(rowsToCheck[, columnName]), ddiVariables[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]])
#'                 }
#'                 rowsToCheck[rowToCheck, columnName] <-
#'                   ddiVariables[[nameBeingChecked]][[as.character(nameBeingChecked)]][[columnName]]
#'               }
#'             }
#'             # Remove that value from the list to avoid repetition during new row creation
#'             ddiVariables[[nameBeingChecked]][[nameBeingChecked]] <-
#'               NULL
#'             finalFrame <-
#'               rbind(finalFrame, rowsToCheck[rowToCheck, ])
#'           }else{
#'             # leave the row untouched if no value is matched
#'             finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck,])
#'           }
#'         } else{
#'           # leave the row untouched if no value is matched
#'           finalFrame <- rbind(finalFrame, rowsToCheck[rowToCheck,])
#'         }
#'       }
#'       
#'       # Create new Rows for leftover data
#'       for (leftOverValue in names(ddiVariables[[nameBeingChecked]])) {
#'         rowToAdd <-  onlyDesiredVariables[0, ]
#'         for (columnName in names(ddiVariables[[nameBeingChecked]][[leftOverValue]])) {
#'           leftOverVariableValue <-
#'             ddiVariables[[nameBeingChecked]][[as.character(leftOverValue)]]
#'           if (!leftOverVariableValue[[columnName]] %in% levels(rowToAdd[, columnName])) {
#'             levels(rowToAdd[, columnName]) <-
#'               c(levels(rowToAdd[, columnName]), leftOverVariableValue[[columnName]])
#'           }
#'           rowToAdd[1, columnName] <-
#'             leftOverVariableValue[[columnName]]
#'         }
#'         
#'         rowToAdd[1, pkg.globals$argument.VariableStart] <-
#'           nameBeingChecked
#'         finalFrame <- rbind(finalFrame, rowToAdd)
#'       }
#'     }
#'     
#'     variablesNotRelatedToTheDDI <-
#'       variable_details[!variable_details$variableStart %in% names(ddiVariables), ]
#'     finalFrame <- rbind(finalFrame, variablesNotRelatedToTheDDI)
#'     rownames(finalFrame) <- NULL
#'     
#'     return(finalFrame)
#'   }
#' 
#' #' Imports DDI metadata into a variable details worksheet
#' #' 
#' #' Updates a variable details worksheet with metadata from a DDI document. New rows
#' #' are added for missing categories and columns that are empty are updated with
#' #' values from the document. No information from the worksheet is overwritten.
#' #'
#' #' @param ddi A string that is the file path to the DDI document
#' #' @param variable_details A data frame containing a variable details worksheet
#' #' @return A dataframe containing the updated variable details worksheet
#' #' @export
#' #' @examples 
#' #' library(bllflow)
#' #' 
#' #' pbcDDI <- bllflow::ReadDDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #' variable_details <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #' 
#' #' populatedDetails <- ProcessDDIVariableDetails(pbcDDI, variable_details)
#' ProcessDDIVariableDetails <- function(ddi, variable_details) {
#'   variableValueList <- list()
#'   ddiVariables <- list()
#'   ddiMetaData <- ddi$variableMetaData
#'   ddiObject <- ddi$ddiObject
#'   # used for parsing out additional data
#'   detected_variables <-
#'     unique(variable_details[pkg.globals$argument.VariableStart])
#'   # Find extra info about the variable low and high
#'   valueForHighLow <- list()
#'   
#'   # Need to loop through every element because the xml2 names all variables var
#'   for (individualVariable in ddiObject$codeBook$dataDscr) {
#'     if (!is.null(attr(individualVariable, "name", exact = TRUE))) {
#'       ddiElementName <- attr(individualVariable, "name", exact = TRUE)
#'       if (length(detected_variables[detected_variables$variableStart == ddiElementName, 1]) != 0) {
#'         valueForHighLow[[ddiElementName]] <- individualVariable$valrng$range
#'         valueForHighLow[[ddiElementName]][["Type"]] <-
#'           ifelse(attr(individualVariable, "intrvl") == "discrete",
#'                  pkg.globals$ddiValueName.Cat,
#'                  pkg.globals$ddiValueName.Cont)
#'       }
#'     }
#'   }
#'   
#'   # Loop through every unique variable found in the VariableDetails
#'   for (variableToCheck in detected_variables[, 1]) {
#'     # Check if that variable is recorded in DDI
#'     if (variableToCheck %in% names(ddiMetaData$dataDscr)) {
#'       # Store the label for that variable
#'       variableInfo <-
#'         ddiMetaData$dataDscr[[variableToCheck]]
#'       variableValueList <- list()
#'       # Check for pressence of value and their labels
#'       if (!is.null(variableInfo$values)) {
#'         for (valueLabelToCheck in names(variableInfo$values)) {
#'           catValue <- variableInfo$values[[valueLabelToCheck]]
#'           variableValueList[[as.character(catValue)]] <-
#'             AddDDIToList(
#'               valueForHighLow[[variableToCheck]]$Type,
#'               catValue,
#'               valueLabelToCheck,
#'               variableInfo$label,
#'               catValue,
#'               catValue
#'             )
#'         }
#'       }
#'       if (valueForHighLow[[variableToCheck]]$Type != pkg.globals$ddiValueName.Cat){
#'       # Record variable info
#'       variableValueList[[as.character(variableToCheck)]] <- 
#'         AddDDIToList(
#'           valueForHighLow[[variableToCheck]]$Type,
#'           NA,
#'           NA,
#'           variableInfo$label,
#'           attr(valueForHighLow[[variableToCheck]], pkg.globals$ddiValue.Min),
#'           attr(valueForHighLow[[variableToCheck]], pkg.globals$ddiValue.Max)
#'         )
#'     }
#'       # add the list of value labels to that variable
#'       ddiVariables[[variableToCheck]] <- variableValueList
#'     }
#'   }
#'   
#'   if (length(ddiVariables) == 0) {
#'     populatedVariableDetails <- NULL
#'   } else{
#'     populatedVariableDetails <-
#'       PopulateVariableDetails(variable_details,
#'                               ddiVariables)
#'   }
#'   
#'   return(populatedVariableDetails)
#' }
#' 
#' #' AddDDI information to a list
#' #'
#' #' @param variableStartType Variable type cont or cat
#' #' @param catStartValue value of the variable being recorded
#' #' @param catStartLabel Label for the value of the variable
#' #' @param variableStartLabel Label for the variable
#' #' @param variableStartLow Min for the variable value
#' #' @param variableStartHigh Max for the variable value
#' #' @return Returns a list containg data on the variables in varList
#' AddDDIToList <- function(variableStartType,
#'                          catStartValue,
#'                          catStartLabel,
#'                          variableStartLabel,
#'                          variableStartLow ,
#'                          variableStartHigh) {
#'   retList <- list()
#'   retList[[pkg.globals$argument.VariableStartType]] <- variableStartType
#'   retList[[pkg.globals$argument.CatStartValue]] <- catStartValue
#'   retList[[pkg.globals$argument.CatStartLabel]] <- catStartLabel
#'   retList[[pkg.globals$argument.VariableStartLabel]] <- variableStartLabel
#'   retList[[pkg.globals$argument.VariableStartHighLow]] <- paste(variableStartLow, ":",variableStartHigh, sep = "")
#'   
#'   return(retList)
#' }
#' 
