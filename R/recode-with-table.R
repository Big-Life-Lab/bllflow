# Environment for custom script functions
#' @title Recode with Table
#'
#' @name rec_w_table
#'
#' @description \code{rec_w_table()} recodes values of variable, where vaiable selection and recoding rules are describe in a reference table (variable_details). Similar to \code{sjmisc::rec()}. Uses the same rule syntax as \code{sjmisc::rec()}, except rules are in a table as opposed to arguments in the function.
#'
#' @seealso \code{sjmisc::rec()}
#' @export
rec_w_table <- function(data_source = NULL, ...) {
  UseMethod("rec_w_table", data_source)
}

#' @title Recode with Table
#'
#' @name rec_w_table.default
#'
#' @details The \code{variable_details} dataframe has the following variables:
#'  \describe{
#'  \item{variable}{name of new (mutated) variable that is recoded}
#'  \item{databaseStart}{name of dataframe with original variables (\code{variableStart}) to be recoded}
#'  \item{variableStart}{name of variable to be recoded}
#'  \ifem{fromType}{variable type of \code{variableStart}. \code{cat} = categorical or factor variable; \code{cont} = continuous variable (real number or integer)}
#'  \item{catlabel}{label for each category of \code{variable}}
#'  \item{...  add remaining variables (headers) here}
#'  }
#'
#'  Each row in \code{variable_details} comprises one category in a newly transformed variable. The rules for each category the new variable are a string in \code{recFrom} and value in \code{recTo}. These recode pairs are the same syntax as \code{sjmisc::rec()}, except in \code{sjmisc::rec()} the pairs are a string for the function attibute \code{rec =}, separated by '\code{=}'. For example in \code{rec_w_table} \code{variable_details$recFrom = 2; variable_details$recTo = 4} is the same as \code{sjmisc::rec(rec = "2=4")}.
#'
#'  the pairs are obtained from the RecFrom and RecTo columns
#'   \describe{
#'     \item{recode pairs}{each recode pair is row. see above example or \code{PBC-variableDetails.csv}
#'     \item{multiple values}{multiple old values that should be recoded into a new single value may be separated with comma, e.g. \code{recFrom = "1,2"; recTo = 1}}
#'     \item{value range}{a value range is indicated by a colon, e.g. \code{recFrom= "1:4"; recTo = 1} (recodes all values from 1 to 4 into 1}
#'     \item{value range for doubles}{for double vectors (with fractional part), all values within the specified range are recoded; e.g. \code{recFrom = "1:2.5'; recTo = 1} recodes 1 to 2.5 into 1, but 2.55 would not be recoded (since it's not included in the specified range)}
#'     \item{\code{"min"} and \code{"max"}}{minimum and maximum values are indicates by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}), e.g. \code{recFrom = "min:4"; recTo = 1} (recodes all values from minimum values of \code{x} to 4 into 1)}
#'     \item{\code{"else"}}{all other values, which have not been specified yet, are indicated by \emph{else}, e.g. \code{recFrom = "else"; recTo = NA} (recode all other values (not specified in other rows) to "NA")}
#'     \item{\code{"copy"}}{the \code{"else"}-token can be combined with \emph{copy}, indicating that all remaining, not yet recoded values should stay the same (are copied from the original value), e.g. \code{recFrom = "else"; recTo = "copy"}
#'     \item{\code{NA}'s}{\code{\link{NA}} values are allowed both as old and new value, e.g. \code{recFrom "NA"; recTo = 1. or "recFrom = "3:5"; recTo = "NA"} (recodes all NA into 1, and all values from 3 to 5 into NA in the new variable)}
#'
#'
#' @param data_source A dataframe containing the variables to be recoded.
#' @param variable_details A dataframe containing the specifications (rules) for recoding.
#' @param dataset_name String, the name of the dataset containing the variables to be recoded.
#' @param else_value Value (string, number, integer, logical or NA) that is used to replace any values that are outside the specified ranges (no rules for recoding).
#' @param append_to_data Logical, if \code{TRUE} (default), recoded variables will be appended to the data_source.
#' @param log Logical, if \code{FALSE} (default), a log of recoding will not be printed.
#' @param print_note Logical, if \code{FALSE} (default), will not print the content inside the `Note`` column of the variable beinng recoded.
#' @param append_non_DB_columns Logical, if \code{FALSE} (default), will not append variables if missing in `data_source`` but present in `variable_details`.
#' @param variables character vector containing variable names to recode or a variables csv containing additional variable info
#' @param labels named character vector of variable and their label
#'
#' @return a dataframe that is recoded according to rules in variable_details.
#' @export
rec_w_table.default <-
  function(data_source,
           variable_details,
           dataset_name,
           else_value = NA,
           append_to_data = FALSE,
           log = FALSE,
           print_note = TRUE,
           variables = NULL,
           var_labels = NULL,
           custom_function_path = NULL) {
    # If custom Functions are passed create new environment and source
    if (!is.null(custom_function_path)) {
      source(custom_function_path)
    }
    
    # ---- Step 1: Detemine if the passed data is a list or single database
    append_non_DB_columns <- FALSE
    if (class(data_source) == "list" &&
        length(dataset_name) == length(data_source)) {
      for (data_name in dataset_name) {
        # ---- Step 2A: Verify that the passed name exists in the passed data
        
        if (!is.null(data_source[[data_name]])) {
          data_source[[data_name]] <-  recode_call(
            variables = variables,
            data_source = data_source[[data_name]],
            dataset_name = dataset_name,
            print_note = print_note,
            else_value = else_value,
            variable_details = variable_details,
            append_to_data = append_to_data,
            append_non_DB_columns = append_non_DB_columns,
            log = log,
            var_labels = var_labels
          )
        } else{
          stop(
            paste(
              "The data",
              data_name,
              "is missing from the passed list please verify the names are correct in the data_source list and the dataset_name list"
            )
          )
        }
      }
      
    } else if ("data.frame" %in% class(data_source) &&
               length(dataset_name) == 1) {
      data_source <-  recode_call(
        variables = variables,
        data_source = data_source,
        dataset_name = dataset_name,
        print_note = print_note,
        else_value = else_value,
        variable_details = variable_details,
        append_to_data = append_to_data,
        append_non_DB_columns = append_non_DB_columns,
        log = log,
        var_labels = var_labels
      )
    } else{
      stop(
        paste(
          "The passed number of data does not match the passed number of dataNames please verify that the number of databases matches number of passed names.
          Aborting operation!"
        ),
        call. = FALSE
        )
    }
    
    return(data_source)
  }

# Creates inputs and runs recode functions
recode_call <-
  function(variables,
           data_source,
           dataset_name,
           print_note,
           else_value,
           variable_details,
           append_to_data ,
           append_non_DB_columns,
           log,
           var_labels) {
    variable_details[[pkg.globals$argument.Variables]] <- trimws(variable_details[[pkg.globals$argument.Variables]])
    if (!is.null(variables) && "data.frame" %in% class(variables)) {
      variable_details <-
        update_variable_details_based_on_variable_sheet(variableSheet = variables, variable_details = variable_details)
    } else {
      if (!is.null(variables)) {
        variable_details <-
          variable_details[trimws(variable_details[[pkg.globals$argument.Variables]]) %in% variables, ]
        vars_being_recoded <-
          as.character(unique(variable_details[[pkg.globals$argument.Variables]]))
        if (length(vars_being_recoded) != length(variables)) {
          missing_vars <- setdiff(variables, vars_being_recoded)
          warning(
            paste(missing_vars,
            "is missing from variable details therefore cannot be recoded")
          )
        }
      }
      if (is.null(variable_details[[pkg.globals$argument.VariableLabel]])) {
        variable_details[[pkg.globals$argument.VariableLabel]] <- NA
      }
      if (is.null(variable_details[[pkg.globals$argument.VariableLabelShort]])) {
        variable_details[[pkg.globals$argument.VariableLabelShort]] <- NA
      }
    }
    if (!is.null(var_labels)) {
      if (is.null(names(var_labels))) {
        stop(
          "The passed labels was not a named vector please follow the c(var_name = varLalbel) format"
        )
      } else{
        if (is.factor(variable_details[[pkg.globals$argument.VariableLabelShort]])) {
          variable_details[[pkg.globals$argument.VariableLabelShort]] <-
            as.character(variable_details[[pkg.globals$argument.VariableLabelShort]])
        }
        for (var_name in names(var_labels)) {
          variable_details[variable_details[[pkg.globals$argument.Variables]] == var_name, pkg.globals$argument.VariableLabelShort] <-
            var_labels[[var_name]]
        }
      }
    }
    
    all_possible_var_names <-
      unique(as.character(variable_details[[pkg.globals$argument.Variables]]))
    all_variables_detected <-
      variable_details[grepl(dataset_name , variable_details[[pkg.globals$argument.DatabaseStart]]), ]
    
    rec_data <-
      recode_columns(
        data_source = data_source,
        variables_to_process = all_variables_detected,
        data_name = dataset_name,
        log = log,
        print_note = print_note,
        else_default = else_value
      )
    if (append_non_DB_columns) {
      missed_variables <-
        all_possible_var_names[!all_possible_var_names %in% unique(as.character(all_variables_detected[, pkg.globals$argument.Variables]))]
      for (missed_variable_name in missed_variables) {
        rec_data[[missed_variable_name]] <- NA
      }
    }
    
    if (append_to_data) {
      data_source <- cbind(data_source, rec_data)
    } else{
      data_source <- rec_data
    }
    
    return(data_source)
  }

#' @title Get Data Variable Name
#'
#' @name get_data_variable_name
#'
#' @description Retrieves the name of the column inside data_source to use for calculations
#'
#' @param data_name name of the database being checked
#' @param row_being_checked the row from variable details that contains information on this variables
#' @param variable_being_checked the name of the recoded variable
#'
#' @return the data_source equivalant of variable_being_checked
get_data_variable_name <-
  function(data_name,
           data,
           row_being_checked,
           variable_being_checked) {
    data_variable_being_checked <- character()
    var_start_names <-
      as.character(row_being_checked[[pkg.globals$argument.VariableStart]])
    
    if (grepl(data_name, var_start_names)) {
      var_start_names_list <- as.list(strsplit(var_start_names, ",")[[1]])
      # Find exact var Name
      for (var_name in var_start_names_list) {
        if (grepl(data_name, var_name)) {
          # seperate dataname from the var name
          data_variable_being_checked <-
            as.list(strsplit(var_name, "::")[[1]])[[2]]
        }
      }
    } else if (grepl("\\[", var_start_names)) {
      data_variable_being_checked <-
        stringr::str_match(var_start_names, "\\[(.*?)\\]")[, 2]
    } else{
      stop(
        paste(
          "The row
          ",
          row,
          "for the variable",
          variable_being_checked,
          "
          Does not contain the database being checked(",
          data_name,
          ") in its variable start the default is also missing.
          Please double check if this variable should have this",
          data_name,
          "included in its databaseStart"
          )
        )
    }
    
    return(data_variable_being_checked)
  }

#' recode_columns
#'
#' Recodes columns from passed row returns just table with those columns and same rows as the data_source
#'
#' @param data_source The source database
#' @param variables_to_process rows from variable details that are applicable to this DB
#' @param log The option of printing log
#' @param print_note the option of printing the note columns
#'
#' @return Returns recoded and labeled data
recode_columns <-
  function(data_source,
           variables_to_process,
           data_name,
           log,
           print_note,
           else_default) {
    # Split variables to process into recode map and func
    map_variables_to_process <-
      variables_to_process[grepl("map::", variables_to_process[[pkg.globals$argument.CatValue]]), ]
    
    func_variables_to_process <-
      variables_to_process[grepl("Func::", variables_to_process[[pkg.globals$argument.CatValue]]), ]
    
    rec_variables_to_process <-
      variables_to_process[!grepl("Func::|map::", variables_to_process[[pkg.globals$argument.CatValue]]), ]
    
    label_list <- list()
    # Set interval if none is present
    intervalPresent <- TRUE
    validIntervals <- c("[,]", "[,)", "(,]")
    intervalDefault <- "[,)"
    recodedData <- data_source[, 0]
    if (is.null(rec_variables_to_process[[pkg.globals$argument.Interval]])) {
      intervalPresent <- FALSE
    }
    
    # Loop through the rows of recode vars
    while (nrow(rec_variables_to_process) > 0) {
      variable_being_checked <-
        as.character(rec_variables_to_process[1, pkg.globals$argument.Variables])
      rowsBeingChecked <-
        rec_variables_to_process[rec_variables_to_process[[pkg.globals$argument.Variables]] == variable_being_checked, ]
      rec_variables_to_process <-
        rec_variables_to_process[!rec_variables_to_process[[pkg.globals$argument.Variables]] == variable_being_checked, ]
      firstRow <- rowsBeingChecked[1,]
      # Check for varialbe existance in data
      data_variable_being_checked <-
        get_data_variable_name(
          data_name = data_name,
          row_being_checked = firstRow,
          variable_being_checked = variable_being_checked,
          data = data_source
        )
      if (is.null(data_source[[data_variable_being_checked]])) {
        warning(
          paste(
            "Data",
            data_name,
            "does not contain the variable",
            data_variable_being_checked
          )
        )
      } else{
        # Check for From column duplicates
        allFromValuesForVariable <-
          rowsBeingChecked[[pkg.globals$argument.From]]
        if (length(unique(allFromValuesForVariable)) != length(allFromValuesForVariable)) {
          for (singleFrom in allFromValuesForVariable) {
            if (sum(allFromValuesForVariable == singleFrom) > 1) {
              stop(
                paste(
                  singleFrom,
                  "was detected more then once in",
                  variable_being_checked,
                  "please make sure only one from value is being recoded"
                )
              )
            }
          }
        }
        
        # Set factor for all recode values
        label_list[[variable_being_checked]] <-
          CreateLabelListElement(rowsBeingChecked)
        else_value <-
          as.character(rowsBeingChecked[rowsBeingChecked[[pkg.globals$argument.From]] == "else", pkg.globals$argument.CatValue])
        if (length(else_value) == 1 &&
            !isEqual(else_value, "character(0)")) {
          else_value <-
            RecodeVariableNAFormating(else_value, label_list[[variable_being_checked]]$type)
          if (isEqual(else_value, "copy")) {
            data_variable_being_checked <-
              get_data_variable_name(
                data_name = data_name,
                row_being_checked = firstRow,
                variable_being_checked = variable_being_checked,
                data = data_source
              )
            recodedData[variable_being_checked] <-
              data_source[data_variable_being_checked]
          } else {
            recodedData[variable_being_checked] <- else_value
          }
          # Catch multiple else rows
        } else if (length(else_value) > 1) {
          stop(
            paste(
              variable_being_checked,
              " contains",
              length(else_value),
              "rows of else only one else value is allowed"
            )
          )
        }
        else{
          recodedData[variable_being_checked] <- else_default
        }
        rowsBeingChecked <-
          rowsBeingChecked[!rowsBeingChecked[[pkg.globals$argument.From]] == "else", ]
        if (nrow(rowsBeingChecked) > 0) {
          logTable <- rowsBeingChecked[, 0]
          logTable$valueTo <- NA
          logTable$From <- NA
          logTable$rowsRecoded <- NA
          levels(recodedData[[variable_being_checked]]) <-
            c(levels(recodedData[[variable_being_checked]]), levels(rowsBeingChecked[[pkg.globals$argument.CatValue]]))
          
          for (row in 1:nrow(rowsBeingChecked)) {
            row_being_checked <- rowsBeingChecked[row,]
            # If cat go check for label and obtain it
            
            # regardless obtain unit and attach
            
            # find var name for this database
            data_variable_being_checked <-
              get_data_variable_name(
                data_name = data_name,
                row_being_checked = row_being_checked,
                variable_being_checked = variable_being_checked,
                data = data_source
              )
            
            # Recode the variable
            fromValues <- list()
            if (grepl(":", as.character(row_being_checked[[pkg.globals$argument.From]]))) {
              fromValues <-
                strsplit(as.character(row_being_checked[[pkg.globals$argument.From]]), ":")[[1]]
            } else {
              # TODO find a more elagant way to handle in the future
              tempFrom <-
                as.character(row_being_checked[[pkg.globals$argument.From]])
              fromValues[[1]] <- tempFrom
              fromValues[[2]] <- fromValues[[1]]
            }
            valueRecorded <-
              as.character(row_being_checked[[pkg.globals$argument.CatValue]])
            if (intervalPresent) {
              interval = as.character(row_being_checked[[pkg.globals$argument.Interval]])
              if (!interval %in% validIntervals) {
                interval <- intervalDefault
              }
              if (fromValues[[1]] == fromValues[[2]]) {
                interval <- "[,]"
              }
              validRowIndex <- CompareValueBasedOnInterval(
                compareColumns = data_variable_being_checked,
                data_source = data_source,
                leftBoundary = fromValues[[1]],
                rightBoundary = fromValues[[2]],
                interval = interval
              )
            } else{
              if (fromValues[[1]] == fromValues[[2]]) {
                interval <- "[,]"
              } else{
                interval <- intervalDefault
              }
              validRowIndex <- CompareValueBasedOnInterval(
                compareColumns = data_variable_being_checked,
                data_source = data_source,
                leftBoundary = fromValues[[1]],
                rightBoundary = fromValues[[2]],
                interval = interval
              )
            }
            # Start construction of dataframe for log
            logTable[row, "valueTo"] <- valueRecorded
            logTable[row, "From"] <-
              as.character(row_being_checked[[pkg.globals$argument.From]])
            logTable[row, "rowsRecoded"] <-
              sum(validRowIndex, na.rm = TRUE)
            
            valueRecorded <-
              RecodeVariableNAFormating(valueRecorded, label_list[[variable_being_checked]]$type)
            if (isEqual(valueRecorded, "copy")) {
              valueRecorded <-
                data_source[validRowIndex, data_variable_being_checked]
            }
            recodedData[validRowIndex, variable_being_checked] <-
              valueRecorded
            if (print_note &&
                !is.null(row_being_checked[[pkg.globals$argument.Notes]]) &&
                !isEqual(row_being_checked[[pkg.globals$argument.Notes]], "") &&
                !is.na(row_being_checked[[pkg.globals$argument.Notes]])) {
              print(paste("NOTE:", as.character(row_being_checked[[pkg.globals$argument.Notes]])))
            }
          }
          # if log was requested print it
          if (log) {
            print(
              paste(
                "The variable",
                data_variable_being_checked,
                "was recoded into",
                variable_being_checked,
                "for the database",
                data_name,
                "the following recodes were made:"
              )
            )
            # Reset rowCount to avoid confusion
            rownames(logTable) <- NULL
            print(logTable)
          }
        }
        
      }
    }
    
    # Process funcVars
    while (nrow(func_variables_to_process) > 0) {
      firstRow <- func_variables_to_process[1, ]
      firstRowVariableName <-
        as.character(firstRow[[pkg.globals$argument.Variables]])
      # get name of var pass to
      derivedReturn <-
        RecodeDerivedVariables(
          variableBeingProcessed = firstRowVariableName,
          recodedData = recodedData,
          variables_to_process = func_variables_to_process,
          log = log,
          print_note = print_note,
          else_default = else_default,
          label_list = label_list,
          varStack = c()
        )
      label_list <- derivedReturn$label_list
      recodedData <- derivedReturn$recodedData
      func_variables_to_process <- derivedReturn$variables_to_process
    }
    # Populate data Labels
    recodedData <-
      LabelData(label_list = label_list, dataToLabel = recodedData)
    
    return(recodedData)
  }

#' Compare Value Based On Interval
#'
#' Compare values on the scientific notation interval
#'
#' @param leftBoundary the min value
#' @param rightBoundary the max value
#' @param data_source the data that contains values being compared
#' @param compareColumns The columns inside data_source being checked
#' @param interval The scientific notation interval
#'
#' @return a boolean vector containing true for rows where the comparison is true
CompareValueBasedOnInterval <-
  function(leftBoundary,
           rightBoundary,
           data_source,
           compareColumns,
           interval) {
    returnBoolean <- vector()
    if (suppressWarnings(is.na(as.numeric(leftBoundary)))) {
      returnBoolean <-
        data_source[[compareColumns]] %in% data_source[[compareColumns]][which(leftBoundary == data_source[[compareColumns]])]
    } else{
      if (interval == "[,]") {
        returnBoolean <-
          data_source[[compareColumns]] %in% data_source[[compareColumns]][which(
            as.numeric(leftBoundary) <= data_source[[compareColumns]] &
              data_source[[compareColumns]] <= as.numeric(rightBoundary)
          )]
      } else if (interval == "[,)") {
        returnBoolean <-
          data_source[[compareColumns]] %in% data_source[[compareColumns]][which(
            as.numeric(leftBoundary) <= data_source[[compareColumns]] &
              data_source[[compareColumns]] < as.numeric(rightBoundary)
          )]
      } else if (interval == "(,]") {
        returnBoolean <-
          data_source[[compareColumns]] %in% data_source[[compareColumns]][which(
            as.numeric(leftBoundary) < data_source[[compareColumns]] &
              data_source[[compareColumns]] <= as.numeric(rightBoundary)
          )]
      } else{
        stop("Invalid Argument was passed")
      }
    }
    
    return(returnBoolean)
  }

# Parse out variables csv
update_variable_details_based_on_variable_sheet <-
  function(variableSheet, variable_details) {
    # remove conflicting columns from variable details
    variable_details <-
      variable_details[,!(
        names(variable_details) %in% c(
          pkg.globals$MSW.Variables.Columns.VariableType,
          pkg.globals$MSW.Variables.Columns.Label,
          pkg.globals$MSW.Variables.Columns.LabelLong,
          pkg.globals$MSW.Variables.Columns.Units
        )
      )]
    # Only keep the needed columns
    variableSheet <-
      variableSheet[, c(
        pkg.globals$MSW.Variables.Columns.Variable,
        pkg.globals$MSW.Variables.Columns.VariableType,
        pkg.globals$MSW.Variables.Columns.Label,
        pkg.globals$MSW.Variables.Columns.LabelLong,
        pkg.globals$MSW.Variables.Columns.Units
      )]
    # merge the labels and data
    variable_details <-
      merge(
        variable_details,
        variableSheet,
        by.x = pkg.globals$argument.Variables,
        by.y = pkg.globals$MSW.Variables.Columns.Variable,
        all.x = TRUE
      )
    # remove variables not present in variableSheet
    variable_details <-
      variable_details[variable_details[[pkg.globals$argument.Variables]] %in% variableSheet[[pkg.globals$MSW.Variables.Columns.Variable]], ]
    
    return(variable_details)
  }

RecodeVariableNAFormating <- function(cellValue, varType) {
  recodeValue <- NULL
  if (grepl("NA", cellValue)) {
    naValueList <- strsplit(cellValue, ":")[[1]]
    if (isEqual(varType, pkg.globals$argument.CatType)) {
      recodeValue <- paste("NA(", naValueList[[3]], ")", sep = "")
    } else{
      recodeValue <- haven::tagged_na(as.character(naValueList[[3]]))
    }
  } else{
    if (!isEqual(varType, pkg.globals$argument.CatType) &&
        !isEqual(cellValue, "copy")) {
      cellValue <- as.numeric(cellValue)
    }
    recodeValue <- cellValue
  }
  
  return(recodeValue)
}

RecodeDerivedVariables <-
  function(recodedData,
           variableBeingProcessed,
           variables_to_process,
           log,
           print_note,
           else_default,
           label_list,
           varStack) {
    if (nrow(variables_to_process) <= 0) {
      stop(paste(variableBeingProcessed, "is missing from variable_details"))
    }
    varStack <- c(varStack, variableBeingProcessed)
    # obtain rows to process and updated variables to Process
    variableRows <-
      variables_to_process[variables_to_process[[pkg.globals$argument.Variables]] == variableBeingProcessed, ]
    variables_to_process <-
      variables_to_process[variables_to_process[[pkg.globals$argument.Variables]] != variableBeingProcessed, ]
    for (rowNum in 1:nrow(variableRows)) {
      # Check for presence of feeder variables in data and in the variable being processed stack
      feederVars <-
        as.list(strsplit(as.character(variableRows[rowNum, ][[pkg.globals$argument.VariableStart]]), "::"))[[1]][[2]]
      feederVars <- gsub("\\[|\\]", "", feederVars)
      feederVars <- as.list(strsplit(feederVars, ","))[[1]]
      feederVars <- sapply(feederVars, trimws)
      usedFeederVars <- feederVars
      feederVars <- setdiff(feederVars, names(recodedData))
      
      # Check if the variable has a function to recode
      nonFuncMissingVariables <-
        setdiff(feederVars, unique(as.character(variables_to_process[[pkg.globals$argument.Variables]])))
      if (length(nonFuncMissingVariables) > 0) {
        warning(
          paste(
            variableBeingProcessed,
            "could not be calculated because",
            feederVars,
            "was never recoded and is not a function variable"
          )
        )
        varStack <- varStack[!(varStack == variableBeingProcessed)]
        
        return(
          list(
            varStack = varStack,
            label_list = label_list,
            recodedData = recodedData,
            variables_to_process = variables_to_process
          )
        )
      }
      # Check for presense in varStack
      if (length(intersect(feederVars, varStack)) > 0) {
        conflictVars <- intersect(feederVars, varStack)
        stop(
          paste(
            conflictVars,
            "is required to create",
            variableBeingProcessed,
            "but",
            variableBeingProcessed,
            "is needed to create",
            conflictVars
          )
        )
      }
      
      # Update varStack and recurse to get the feeder vars
      for (oneFeeder in feederVars) {
        # Need to check recoded data again in case a recursion added it
        if (!oneFeeder %in% names(recodedData)) {
          derivedReturn <-
            RecodeDerivedVariables(
              variableBeingProcessed = oneFeeder,
              recodedData = recodedData,
              variables_to_process = variables_to_process,
              log = log,
              print_note = print_note,
              else_default = else_default,
              label_list = label_list,
              varStack = varStack
            )
          varStack <- derivedReturn$varStack
          label_list <- derivedReturn$label_list
          recodedData <- derivedReturn$recodedData
          variables_to_process <- derivedReturn$variables_to_process
        }
      }
      
      # Obtain the function for each row
      append(label_list, CreateLabelListElement(variableRows))
      
      row_being_checked <- variableRows[rowNum,]
      funcCell <-
        as.character(row_being_checked[[pkg.globals$argument.CatValue]])
      functionBeingUsed <-
        as.list(strsplit(funcCell, "::"))[[1]][[2]]
      
      columnValue <-
        recodedData %>% dplyr::rowwise(.) %>% dplyr::select(usedFeederVars) %>%
        dplyr::do(
          columnBeingAdded = CalculateCustomFunctionRowValue(
            .,
            variableNames = usedFeederVars,
            customFunctionName = functionBeingUsed
          )
        )
      recodedData[[variableBeingProcessed]] <-
        unlist(columnValue[["columnBeingAdded"]])
      
      varStack <- varStack[!(varStack == variableBeingProcessed)]
    }
    
    return(
      list(
        varStack = varStack,
        label_list = label_list,
        recodedData = recodedData,
        variables_to_process = variables_to_process
      )
    )
    
  }
CalculateCustomFunctionRowValue <-
  function(rowValues,
           variableNames,
           customFunctionName) {
    rowValues <- unname(rowValues)
    # TODO add ability to process ranges in from column
    customFunctionReturnValue <-
      do.call(get(customFunctionName), rowValues)
    
    return(customFunctionReturnValue)
  }
