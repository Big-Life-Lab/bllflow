source(file.path(getwd(), 'dummy-variable.R'))

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
    currentDummyVariableCode <- getCodeForDummyVariable(variableNameSplitWithUnderscore[[1]], numberOfCategories)
    dummyVariableCode <- paste(dummyVariableCode, currentDummyVariableCode, sep = '')
  }
  if (dir.exists(projectFolderPath) == FALSE) {
    dir.create(projectFolderPath)
  }
  cat(dummyVariableCode, file = paste(projectFolderPath, '/dummy-variables.R', sep = ''))
}