source(file.path(getwd(), 'R/dummy-variable.R'))
source(file.path(getwd(), 'R/centered-variable.R'))
source(file.path(getwd(), 'R/interactions.R'))

VariableNameColumName <- 'variableName'
CategoriesColumnName <- 'categories'
CenterColumnName <- 'centre'
DummyFlagColumnName <- 'dummy'
CsvYesValue <- 'yes'

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
  variablesToDummy <- getVariablesToDummy(webSpecCsv)
  # Will have all the dummying code
  dummyVariableCode <- ''
  if(nrow(variablesToDummy) != 0) {
    for (i in 1:nrow(variablesToDummy)) {
      numberOfCategories <- as.numeric(variablesToDummy[i, CategoriesColumnName])
      currentDummyVariableCode <- getCodeForDummyVariable(variablesToDummy[i, VariableNameColumName], numberOfCategories)
      
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
  variablesToCenter <- getVariablesToCenter(webSpecCsv)
  # Go through each web spec row
  for (i in 1:nrow(variablesToCenter)) {
    variableName <- variablesToCenter[i, VariableNameColumName]
    
    # If the current row is one we have to dummy
    if (variablesToCenter[i, DummyFlagColumnName]==CsvYesValue) {
      # Cast to numeric since it has a NA value in it
      numberOfCategories <- as.numeric(variablesToCenter[i, CategoriesColumnName])
      
      # Add centering code for each dummied var
      for (j in 1:numberOfCategories) {
        # In RESPECT the convention we use is:
        # 2. Dummied cat vars have _<category number> in them
        # 3. Use the above 2 facts to pass in the right values for the 
        #    getCodeForCenteredVariable function
        centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(glue::glue('{variableName}_{j}')), sep = '\n')
      }
      
      centeredVariableCode <- paste(centeredVariableCode, '', sep = '\n')
    }
    # Otherwise it's a continuous variable
    else {
      centeredVariableCode <- paste(centeredVariableCode, getCodeForCenteredVariable(variableName), sep = '\n\n')
    } 
  }
  
  cat(dummyVariableCode, file = paste(projectFolderPath, '/dummy-variables.R', sep = ''))
  cat(centeredVariableCode, file = paste(projectFolderPath, '/centered-variables.R', sep = ''))
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

getCategoryVariables <- function(webSpec) {
  return(webSpec[which(is.na(webSpec[, CategoriesColumnName])==FALSE), ])
}

getVariablesToDummy <- function(webSpec) {
  return(webSpec[which(webSpec[, DummyFlagColumnName]==CsvYesValue), ])
}

getVariablesToCenter <- function(webSpec) {
  return(webSpec[which(webSpec[, CenterColumnName] == CsvYesValue), ])
}

buildProjectFiles(file.path(getwd(), 'test-assets/respect-web-spec.csv'), file.path(getwd(), 'generated-project'))