library(glue)

#' Checks whether an interactions is already present in the interactionsList arg
#'
#' @param interactionsList list[list[string, string]] The interactions to check
#' @param varOne One of the variables part of the interaction
#' @param varTwo The second var part of the interaction
#'
#' @return boolean
#' @export
#'
#' @examples
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

#' Builds the interactions list for the provided rows
#' 
#' @param rowsWithInteractions A webspec dataframe that contains only
#' interactions rows
#'
#' @return list[list] A list where each item is a 2 item list containing the variables
#' being interacted
#' @export
#'
#' @examples
buildInteractionsList <- function(rowsWithInteractions) {
  # The list of interactions to return
  interactionsList <- list()

  for(i in 1:nrow(rowsWithInteractions)) {
    # Get the variables this this var row will interact with
    interactionsForCurrentRow <- strsplit(rowsWithInteractions[i, 'Interactions'], ',')
    
    # Go through the each interaction for the current var and add it to the
    # interactionsList if it hasn't already been done
    lapply(interactionsForCurrentRow[[1]], function(interactionVarTwo) {
      trimmedInteractionVarTwo <- trimws(interactionVarTwo)
      
      if(isInInteractionsList(interactionsList, rowsWithInteractions[i, 'Variable_Name'], trimmedInteractionVarTwo) == FALSE) {
        interactionsList[[length(interactionsList)+1]] <<- list(rowsWithInteractions[i, 'Variable_Name'], trimmedInteractionVarTwo)
      } 
    })
  }

  return(interactionsList)
}

#' Formats a variable name for interaction
#'
#' @param varName The var name to format. This is the not the entire interaction
#' var string but just one of the variables part of the interaction
#'
#' @return string
#' @export
#'
#' @examples
formatInteractionVariable <- function(varName) {
  # Removed _cont and _cat from the varName
  return(gsub('_cont', '', gsub('_cat', '', varName)))  
}

#' Returns code for an interaction
#'
#' @param varOne The first var part of this interaction 
#' @param varTwo Th second var part of this interaction 
#'
#' @return string
#' @export
#'
#' @examples
getCodeForInteraction <- function(varOne, varTwo) {
  formattedVarOne <- formatInteractionVariable(varOne)
  formattedVarTwo <- formatInteractionVariable(varTwo)
  
  # Interaction code calls the Interact.fun between the 2 vars. The new
  # var name is the 3 formatted variable names seperated by a X
  return(glue::glue('{formattedVarOne}X{formattedVarTwo}_int <- Interact.fun({varOne}, {varTwo})'))
}