#' Add an event date, when you only have an event probability
#'
#' To create example data for algorithm development.
#' Use case: You have validation scoring data with event probablity but not
#' event date or censoring date.
#' Use case: You have cohort data with only exposure variables and you would
#' like to create a fictious event or censoring events.
#'
#' @param df (required). Data frame with event propobablity. Each row represents
#'  an observation.
#' @param eventProbability (required) Target event probablity. Event probability
#' e.g. 5-year risk of death is 0.2 represents 20% proabality of dying over
#' 5 years or 1825 days.
#' @param followUpTime (required) Time period for event probablity
#' @return List with event times. Returned list length = list length of df. Event
#' date =  totalTime (censored), if no event.
#' @param eventLabel (optional) Name of event. If missing, default to 'Time to
#' event'.
#' @param eventUnit (optional) Time unit for event. e.g. days
#' @param probablityWithdraw (optional) Probablity the observation was lost to
#' follow-up prior to end of follow up (followUpTime)
#' @param probablityCompetingEvent (optional) Probablity the observation was a
#' competing event.
#'
#' @example df$ttEvent <- makeEventDate(df, df$risk, 1825)
#' # each ovservation as event probablity of df$risk
#' @example df$ttcensor <- makeEventDate(df, .01, 1825)
makeEventDate <- function (df, eventProbability, followUpTime) {
  df$r <-
    runif(nrow(df))  # create temporary list of random probablities

  return (ifelse(
    df$r <= eventProbability,
    as.integer(totalTime * df$r),
    totalTime
  ))
}