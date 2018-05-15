source(file.path(getwd(), 'R/project-builder.R'))
source(file.path(getwd(), 'R/dummy-variable.R'))
source(file.path(getwd(), 'R/interactions.R'))

testBuildProjectFiles <- function() {
  webSpecCsvFilePath <- file.path(getwd(), 'test-assets/web-specifications.csv')
  projectFolderPath <- file.path(getwd(), 'generated-project')
  
  buildProjectFiles(webSpecCsvFilePath, projectFolderPath)
  
  # Dummy Variables
  dummyVariablesFilePath <- paste(projectFolderPath, '/dummy-variables.R', sep = '')
  if(!file.exists(dummyVariablesFilePath)) {
    stop('buildProjectFiles: dummy variables file not created')  
  }
  print('buildProjectFiles: dummy variables file created')
  
  actualDummyVariableFileContents <- glue::glue(readChar(dummyVariablesFilePath, file.info(dummyVariablesFilePath)$size))
  
  expectedDummyVariableFileContents <- getCodeForDummyVariable('BB6_cat5', 5)
  expectedDummyVariableFileContents <- glue::glue('{expectedDummyVariableFileContents}\n\n{getCodeForDummyVariable("JA1_cat2", 2)}')
  
  if(actualDummyVariableFileContents != expectedDummyVariableFileContents) {
    print('Actual File Content')
    print(actualDummyVariableFileContents)
    print('Expected File Content')
    print(expectedDummyVariableFileContents)
    stop('buildProjectFiles: Incorrectly generated dummy variable file')
  }
  
  # Interactions variables
  interactionVariablesFilePath <- paste(projectFolderPath, '/interactions.R', sep = '')
  if(!file.exists(interactionVariablesFilePath)) {
    stop('buildProjectFiles: interactions file not created')  
  }
  print('buildProjectFiles: interactions file created')
  
  actualInteractionsFileContents <- glue::glue(readChar(interactionVariablesFilePath, file.info(interactionVariablesFilePath)$size))
  
  expectedInteractionsFileContents <- getCodeForInteraction('AgeCXActiveCancerC_int', c('BB6_cat5', 'JA1_cat2', 'age_rcs4', 'AgeC_cont', 'ActiveCancerC_cat'))
  
  if(actualInteractionsFileContents != expectedInteractionsFileContents) {
    print('Actual File Content')
    print(actualInteractionsFileContents)
    print('Expected File Content')
    print(expectedInteractionsFileContents)
    stop('buildProjectFiles: Incorrectly generated interactions file')
  }
  
  print('buildProjectFiles: Correctly generated interactions file')
}