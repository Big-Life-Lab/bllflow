source(file.path(getwd(), 'R/interactions.R'))

library(glue)

testGetCodeForInteractions <- function() {
  interactionVariableName <- 'AgeCXActiveCancerC_int'
  allVariables <- c('AgeC_cont', 'SmokingStatus', 'ActiveCancerC_cat')
  
  actualCode <- getCodeForInteraction(interactionVariableName, allVariables)
  
  expectedCode <- glue::glue('{interactionVariableName} <- Interact.fun(AgeC_cont, ActiveCancerC_cat)')
  
  if (actualCode != expectedCode) {
    print('Actual Code: ')
    print(actualCode)
    print('Expected Code: ')
    print(expectedCode)
    stop('getCodeForInteraction: Incorrect generated interaction code')
  } else {
    print('getCodeForInteraction: Correctly genereated interaction code')
  }
}

testDummyVariableFunctions <- function() {
  testGetCodeForDummyVariable()
}