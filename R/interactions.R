library(glue)

isInInteractionsList <- function(interactionsList, varOne, varTwo) {
  if(is.null(interactionsList)) {
    return(FALSE)
  }
  
  if(length(interactionsList) == 0) {
    return(FALSE)
  } 
  
  for(i in 1:length(interactionsList)) {
    if(interactionsList[[i]][[1]] == varOne & interactionsList[[i]][[2]] == varTwo) {
      return(TRUE)
    } else if(interactionsList[[i]][[1]] == varTwo & interactionsList[[i]][[2]] == varOne) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

buildInteractionsList <- function(rowsWithInteractions) {
  interactionsList <- list()

  for(i in 1:nrow(rowsWithInteractions)) {
    interactionsForCurrentRow <- strsplit(rowsWithInteractions[i, 'Interactions'], ',')
    
    lapply(interactionsForCurrentRow[[1]], function(interactionVarTwo) {
      trimmedInteractionVarTwo <- trimws(interactionVarTwo)
      
      if(isInInteractionsList(interactionsList, rowsWithInteractions[i, 'Variable_Name'], trimmedInteractionVarTwo) == FALSE) {
        interactionsList[[length(interactionsList)+1]] <<- list(rowsWithInteractions[i, 'Variable_Name'], trimmedInteractionVarTwo)
      } 
    })
  }

  return(interactionsList)
}

formatInteractionVariable <- function(varName) {
  return(gsub('_cont', '', gsub('_cat', '', varName)))  
}

getCodeForInteraction <- function(varOne, varTwo) {
  formattedVarOne <- formatInteractionVariable(varOne)
  formattedVarTwo <- formatInteractionVariable(varTwo)
  
  return(glue::glue('{formattedVarOne}X{formattedVarTwo} <- Interact.fun({varOne}, {varTwo})'))
}