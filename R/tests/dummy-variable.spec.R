source(file.path(getwd(), 'R/dummy-variable.R'))

library(glue)

testGetCodeForDummyVariable <- function() {
  numberOfCategories <- 2
  dummyVariableName <- 'BB6'
  
  actualCode <- getCodeForDummyVariable(dummyVariableName, numberOfCategories)
  
  expectedCode <- glue::glue('BB6_cat1 <- ifelse(BB6 == 1, 1, 0)BB6_cat2 <- ifelse(BB6 == 2, 1, 0)')
  
  if (actualCode != expectedCode) {
    print('Actual Code: ')
    print(actualCode)
    print('Expected Code: ')
    print(expectedCode)
    stop('getCodeForDummyVariable: Incorrect generated dummy variable code')
  } else {
    print('getCodeForDummyVariable: Correctly genereated dummy variable code')
  }
}

testDummyVariableFunctions <- function() {
  testGetCodeForDummyVariable()
}