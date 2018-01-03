source(file.path(getwd(), 'R/project-builder.R'))
source(file.path(getwd(), 'R/dummy-variable.R'))

testBuildProjectFiles <- function() {
  webSpecCsvFilePath <- file.path(getwd(), 'test-assets/web-specifications.csv')
  projectFolderPath <- file.path(getwd(), 'generated-project')
  
  buildProjectFiles(webSpecCsvFilePath, projectFolderPath)
  
  dummyVariablesFilePath <- paste(projectFolderPath, '/dummy-variables.R', sep = '')
  if(!file.exists(dummyVariablesFilePath)) {
    stop('buildProjectFiles: dummy variables file not created')  
  }
  print('buildProjectFiles: dummy variables file created')
  
  actualDummyVariableFileContents <- readChar(dummyVariablesFilePath, file.info(dummyVariablesFilePath)$size)
  
  expectedDummyVariableFileContents <- getCodeForDummyVariable('BB6', 5)
  
  if(actualDummyVariableFileContents != expectedDummyVariableFileContents) {
    print('Actual File Content')
    print(actualDummyVariableFileContents)
    print('Expected File Content')
    print(expectedDummyVariableFileContents)
    stop('buildProjectFiles: Incorrectly generated dummy variable file')
  }
  print('buildProjectFiles: Correctly generated dummy variable file')
}