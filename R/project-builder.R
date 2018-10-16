source(file.path(getwd(), 'R/dummy-variable.R'))
source(file.path(getwd(), 'R/centered-variable.R'))
source(file.path(getwd(), 'R/interactions.R'))

VariableNameColumName <- 'Variable_Name'

#' Builds several R files from the web spec csv file located at the path 
#' specified by the webSpecCsvFilePath argument. The R files are written to the folder whose path
#'is specified in the projectFolderPath arg
#'
#' @param webSpecCsvFilePath The path to the web spec csv file
#' @param projectFolderPath The path to the folder where the R files will be put
#'
#' @return
#' @export
#'
#' @examples
buildProjectFiles <- function(webSpecCsvFilePath, projectFolderPath) {
  webSpecCsv <- read.csv(webSpecCsvFilePath)
  webSpecCsv <- data.frame(lapply(webSpecCsv, as.character), stringsAsFactors = FALSE)
  
  # Gets all the categorical variable rows from the web spec. Categorical
  # variable rows are ones whose VariableNameColumName ends with _cat<number>
  categoricalVariableRows <- webSpecCsv[which(grepl('_cat\\d', webSpecCsv[, VariableNameColumName])), ]
  # Will have all the dummying code
  dummyVariableCode <- ''
  if(nrow(categoricalVariableRows) != 0) {
    for (i in 1:nrow(categoricalVariableRows)) {
      # Split with '_' string to get an array where the first value is the name
      # of the variable and the second value is 'cat<number>'
      variableNameSplitWithUnderscore <- strsplit(categoricalVariableRows[i, VariableNameColumName], '_')[[1]]
      # Split with second value in variableNameSplitWithUnderscore with
      # 'cat' to get the number of categories in thie variables
      # Cast to numeric since it has a NA value in it
      numberOfCategories <- as.numeric(strsplit(variableNameSplitWithUnderscore[[2]], 'cat')[[1]])[2]
      currentDummyVariableCode <- getCodeForDummyVariable(categoricalVariableRows[i, VariableNameColumName], numberOfCategories)
      
      # If this is the first categorical var for whom we are generating
      # dummying code we just append it to the entire dummying code
      if(i == 1) {
        dummyVariableCode <- glue::glue(dummyVariableCode, currentDummyVariableCode)
      }
      # Otherwise add a newlines between it so that there is space between
      # the dummying code for each categorical var
      else {
        dummyVariableCode <- glue::glue('{dummyVariableCode}\n\n{currentDummyVariableCode}')
      }
    }
  }
  
  # Centering code
  centeredVariableCode <- ''
  
  if(isPortWebSpecFile(webSpecCsv) == FALSE) {
    # Go through each web spec row
    for (i in 1:nrow(webSpecCsv)) {
      # If the current row is a categorical variable
      if (grepl('_cat\\d', webSpecCsv[i, VariableNameColumName])) {
        # The following 2 lines get the number of categories for this
        # cat variable
        variableNameSplitWithUnderscore <- strsplit(webSpecCsv[i, VariableNameColumName], '_')[[1]]
        # Cast to numeric since it has a NA value in it
        numberOfCategories <- as.numeric(strsplit(variableNameSplitWithUnderscore[[2]], 'cat')[[1]])[2]
        
        # Add centering code for each dummied var
        for (j in 1:numberOfCategories) {
          # In RESPECT the convention we use is:
          # 1. Centered variables have the _c in the name
          # 2. Dummied cat vars have _<category number> in them
          # 3. Use the above 2 facts to pass in the right values for the 
          #    getCodeForCenteredVariable function
          centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(glue::glue('{webSpecCsv[i, VariableNameColumName]}_{j}'), glue::glue('{webSpecCsv[i, VariableNameColumName]}_{j}_c')), sep = '\n')
        }
      }
      # Otherwise it's a continuous variable
      else {
        centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(glue::glue('{webSpecCsv[i, VariableNameColumName]}'), glue::glue('{webSpecCsv[i, VariableNameColumName]}_c')), sep = '\n')
      } 
    }
  }
  # For a PORT algorithm
  else {
    # Get all the variables which are centered. These are the ones with C_
    # in the name
    centeredVariables <- webSpecCsv[which(grepl('C_', webSpecCsv[, VariableNameColumName]) & grepl('_rcs1', webSpecCsv[, VariableNameColumName]) == FALSE), ]
    # Don't center rcs variables
    centeredVariables <- centeredVariables[which(grepl('_rcs1', centeredVariables[, VariableNameColumName]) == FALSE), ]

    for(i in 1:nrow(centeredVariables)) {
      unCenteredVariableName <- gsub('C_', '_', centeredVariables[i, VariableNameColumName])
      
      centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(unCenteredVariableName, centeredVariables[i, VariableNameColumName]), sep='\n')
    }
  }
  
  # Rcs Code
  rcsCode <- ''
  if(isPortWebSpecFile(webSpecCsv)) {
    # All rcs variable rows
    rcsRows <- webSpecCsv[which(grepl('_rcs1', webSpecCsv[,  VariableNameColumName])), ]
    nonRcsRows <- webSpecCsv[which(grepl('_rcs1', webSpecCsv[, VariableNameColumName]) == FALSE), ]
    
    for(i in 1:nrow(rcsRows)) {
      # Get the name of the var before the RCSing step
      variableNameWithoutRcs1 <- strsplit(rcsRows[i, VariableNameColumName], '_rcs1')[[1]][[1]]
      # Find a variable with the name same as the variableNameWithoutRcs1 var
      variableForRcsRow <- nonRcsRows[which(grepl(variableNameWithoutRcs1, nonRcsRows[, VariableNameColumName])), ]

      if(nrow(variableForRcsRow) == 0) {
        stop(glue::glue('No variable for rcs {rcsRows[i, VariableNameColumnName]}'))
      }
      
      # The RCS code is just a variable rename from the variable before
      # RCSing step to after
      rcsCodeForRcsRow <- glue::glue('{rcsRows[i, VariableNameColumName]} <- {variableForRcsRow[1, VariableNameColumName]}')

      rcsCode <- paste(rcsCode, rcsCodeForRcsRow, sep='\n')
    }
  }
  
  # Interactions code
  interactionsCode <- ''
  # Interactions rows are ones whose Interactions colums are not empty string
  interactionVariableRows <- webSpecCsv[which(webSpecCsv[, 'Interactions'] != ''), ]
  interactionsList <- buildInteractionsList(interactionVariableRows)

  for(i in 1:length(interactionsList)) {
    codeForCurrentInteraction <- getCodeForInteraction(interactionsList[[i]][[1]], interactionsList[[i]][[2]])
    
    interactionsCode <- paste(interactionsCode, codeForCurrentInteraction, sep='\n')
  }
  
  if (dir.exists(projectFolderPath) == FALSE) {
    dir.create(projectFolderPath)
  }
  cat(dummyVariableCode, file = paste(projectFolderPath, '/dummy-variables.R', sep = ''))
  cat(centeredVariableCode, file = paste(projectFolderPath, '/centered-variables.R', sep = ''))
  cat(interactionsCode, file = paste(projectFolderPath, '/interactions.R', sep = ''))
  cat(rcsCode, file = paste(projectFolderPath, '/rcs.R', sep = ''))
}

#' Checks whether a web spec file is for a PORT algorithm or for a RESPECT
#' algorithm
#'
#' @param webSpecs The web spec file rows in a data frame
#'
#' @return boolean
#' @export
#'
#' @examples
isPortWebSpecFile <- function(webSpecs) {
  # If there is a row where the variable name has a 'C_' in it then it's
  # a PORT web spec file
  rowsWithC_ <- webSpecs[which(grepl('C_', webSpecs[, VariableNameColumName])), ]
  
  if(nrow(rowsWithC_) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}