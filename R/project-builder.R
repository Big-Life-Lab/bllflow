source(file.path(getwd(), 'R/dummy-variable.R'))

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
  if (dir.exists(projectFolderPath) == FALSE) {
    dir.create(projectFolderPath)
  }
  cat(dummyVariableCode, file = paste(projectFolderPath, '/dummy-variables.R', sep = ''))
}