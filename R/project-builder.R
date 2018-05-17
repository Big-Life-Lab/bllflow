source(file.path(getwd(), 'R/dummy-variable.R'))
source(file.path(getwd(), 'R/centered-variable.R'))
source(file.path(getwd(), 'R/interactions.R'))

VariableNameColumName <- 'Variable_Name'

buildProjectFiles <- function(webSpecCsvFilePath, projectFolderPath) {
  webSpecCsv <- read.csv(webSpecCsvFilePath)
  webSpecCsv <- data.frame(lapply(webSpecCsv, as.character), stringsAsFactors = FALSE)
  
  categoricalVariableRows <- webSpecCsv[which(grepl('_cat\\d', webSpecCsv[, VariableNameColumName])), ]
  dummyVariableCode <- ''
  if(nrow(categoricalVariableRows) != 0) {
    for (i in 1:nrow(categoricalVariableRows)) {
      variableNameSplitWithUnderscore <- strsplit(categoricalVariableRows[i, VariableNameColumName], '_')[[1]]
      # Cast to numeric since it has a NA value in it
      numberOfCategories <- as.numeric(strsplit(variableNameSplitWithUnderscore[[2]], 'cat')[[1]])[2]
      currentDummyVariableCode <- getCodeForDummyVariable(categoricalVariableRows[i, VariableNameColumName], numberOfCategories)
      
      if(i == 1) {
        dummyVariableCode <- glue::glue(dummyVariableCode, currentDummyVariableCode)
      } else {
        dummyVariableCode <- glue::glue('{dummyVariableCode}\n\n{currentDummyVariableCode}')
      }
    }
  }
  
  centeredVariableCode <- ''
  
  if(isPortWebSpecFile(webSpecCsv) == FALSE) {
    for (i in 1:nrow(webSpecCsv)) {
      if (grepl('_cat\\d', webSpecCsv[i, VariableNameColumName])) {
        variableNameSplitWithUnderscore <- strsplit(webSpecCsv[i, VariableNameColumName], '_')[[1]]
        # Cast to numeric since it has a NA value in it
        numberOfCategories <- as.numeric(strsplit(variableNameSplitWithUnderscore[[2]], 'cat')[[1]])[2]
        
        for (j in 1:numberOfCategories) {
          centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(glue::glue('{webSpecCsv[i, VariableNameColumName]}_{j}'), glue::glue('{webSpecCsv[i, VariableNameColumName]}_{j}_c')), sep = '\n')
        }
      } else {
        centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(glue::glue('{webSpecCsv[i, VariableNameColumName]}'), glue::glue('{webSpecCsv[i, VariableNameColumName]}_c')), sep = '\n')
      } 
    }
  } else {
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
    rcsRows <- webSpecCsv[which(grepl('_rcs1', webSpecCsv[,  VariableNameColumName])), ]
    nonRcsRows <- webSpecCsv[which(grepl('_rcs1', webSpecCsv[, VariableNameColumName]) == FALSE), ]
    
    for(i in 1:nrow(rcsRows)) {
      variableNameWithoutRcs1 <- strsplit(rcsRows[i, VariableNameColumName], '_rcs1')[[1]][[1]]
      variableForRcsRow <- nonRcsRows[which(grepl(variableNameWithoutRcs1, nonRcsRows[, VariableNameColumName])), ]

      if(nrow(variableForRcsRow) == 0) {
        stop(glue::glue('No variable for rcs {rcsRows[i, VariableNameColumnName]}'))
      }
      
      rcsCodeForRcsRow <- glue::glue('{rcsRows[i, VariableNameColumName]} <- {variableForRcsRow[1, VariableNameColumName]}')

      rcsCode <- paste(rcsCode, rcsCodeForRcsRow, sep='\n')
    }
  }
  
  # Interactions code
  interactionsCode <- ''
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

isPortWebSpecFile <- function(webSpecs) {
  rowsWithC_ <- webSpecs[which(grepl('C_', webSpecs[, VariableNameColumName])), ]
  
  if(nrow(rowsWithC_) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}