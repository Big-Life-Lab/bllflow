library('rstudioapi')
library('glue')
source(file.path(getwd(), 'R/project-builder.R'))

rProjectBuilderAddin <- function() {
  webSpecCsvFilePath <- rstudioapi::getActiveDocumentContext()$path
  
  outDirPath <- file.path(getwd(), 'generated-project')
  if (dir.exists(outDirPath) == FALSE) {
    dir.create(outDirPath)
  }
  buildProjectFiles(webSpecCsvFilePath, outDirPath)
  
  print(glue::glue('Done. The generated project should be in the {outDirPath} folder'))
}