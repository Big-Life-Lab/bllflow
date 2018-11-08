#' Add an event date, when you only have an event probability
#'
#' To create example data for algorithm development.
#' Use case: You have validation scoring data with event probablity but not
#' event date or censoring date.
#' Use case: You have cohort data with only exposure variables and you would
#' like to create a fictious event or censoring events.
#'
#' @param df Data frame with event propobablity. Each row represents an
#'  observation.
#' @param eventProbability Target event probablity. Event probability
#' e.g. 5-year risk of death is 0.2 represents 20% proabality of dying over
#' 5 years or 1825 days.
#' @param totalTime Time period for event probablity
#' @return List with event times. Returned list length = list length of df. Event
#' date =  totalTime (censored), if no event.
#' @example df$ttEvent <- makeEventDate(df, df$risk, 1825) # each ovservation
#' has event probablity of df$risk
#' @example df$ttcensor <- makeEventDate(df, .01, 1825)
makeEventDate <- function (df, eventProbability, totalTime) {
  df$r <- runif(nrow(df))

  return (
    ifelse(
      df$r <= eventProbability,
      as.integer(totalTime * df$r),
      totalTime
    )
  )
}