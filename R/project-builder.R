source(file.path(getwd(), 'R/dummy-variable.R'))
source(file.path(getwd(), 'R/centered-variable.R'))

VariableNameColumName <- 'Variable_Name'

buildProjectFiles <- function(webSpecCsvFilePath, projectFolderPath) {
  webSpecCsv <- read.csv(webSpecCsvFilePath)
  webSpecCsv <- data.frame(lapply(webSpecCsv, as.character), stringsAsFactors = FALSE)
  
  categoricalVariableRows <- webSpecCsv[which(grepl('cat', webSpecCsv[, VariableNameColumName])), ]
  dummyVariableCode <- ''
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
  
  centeredVariableCode <- ''
  for (i in 1:nrow(webSpecCsv)) {
    if (grepl('cat', webSpecCsv[i, VariableNameColumName])) {
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
  
  if (dir.exists(projectFolderPath) == FALSE) {
    dir.create(projectFolderPath)
  }
  cat(dummyVariableCode, file = paste(projectFolderPath, '/dummy-variables.R', sep = ''))
  cat(centeredVariableCode, file = paste(projectFolderPath, '/centered-variables.R', sep = ''))
}