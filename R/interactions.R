library(glue)

getCodeForInteraction <- function(interactionName, allVariables) {
  interactionVariables <- strsplit(gsub("_int", "", interactionName), 'X')[[1]]
  
  originalInteractionVariables <- c()

  for(i in 1:length(interactionVariables)) {
    foundOrigInteractionVariable <- ''
    
    for(j in 1:length(allVariables)) {
      if(grepl(interactionVariables[i], allVariables[j])) {
        foundOrigInteractionVariable <- allVariables[j]
        break
      }
    }
    
    if(foundOrigInteractionVariable == '') {
      stop(glue::glue('No interaction variable found for {interactionVariables[i]}'))
    } else {
      originalInteractionVariables <- c(originalInteractionVariables, foundOrigInteractionVariable)
    }
  }
  
  return(glue::glue('{interactionName} <- Interact.fun({originalInteractionVariables[1]}, {originalInteractionVariables[2]})'))
}