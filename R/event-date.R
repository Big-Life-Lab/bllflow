library(Hmisc) # for variable labels
#' Add an event date, when you only have an event probability
#'
#' @description 
#' To create example data for algorithm development.
#' 
#' Use case: You have validation scoring data with event probablity but not
#' event date or censoring date.
#' 
#' Use case: You have cohort data with only exposure variables and you would
#' like to create a fictious event or censoring events.
#' @param df (required) Data frame with event propobablity. Each row represents
#'  an observation.
#' @param eventprobability (required) Target event probablity. Event probability
#' e.g. 5-year risk of death is 0.2 represents 20 \% proabality of dying over
#' 5 years or 1825 days.
#' @param followuptime (required) Time period for event probablity
#' @param eventLabel (optional) Name of event. If missing, default to 'Time to
#' event'.
#' @param eventUnit (optional) Time unit for event. e.g. days
#' @param probablityWithdraw (optional) Probablity the observation was lost to
#' follow-up prior to end of follow up (followuptime)
#' @param probablityCompetingEvent (optional) Probablity the observation was a
#' competing event.
#' @param label Internal function from Hmisc used for adding labels.
#' #' @return List with event times. Returned list length = list length of df. Event
#' date =  totalTime (censored), if no event.
#' @keywords internal label
#' @examples
#' # load the test data in the bllFlow package
#' \dontrun{
#'  df <- as.data.frame(read.csv("inst/extdata/RESPECT-EOL_validation.csv"))
#'  
#'  # or 
#'  # df <- RESPECT-EOL_validation
#'  
#' # each ovservation as event probablity of df$risk
#'  # df$ttEvent <- eventdate(df, df$risk, 1825)
#'  add variable labeles
#'  # df$ttEvent <- eventdate(df, df$risk, 1825, label = "time to death (all-cause)")
#' #check the label
#'  # label(df$ttEvent)
#'
#'  # df$ttcensor <- eventdate(df, .01, 1825)
#' }
#' @export eventdate
eventdate <- function (df, eventprobability, followuptime,
                       label = "Time to event") {
  #if !eventLabel = "Time to event"
  randomrisk <-
    runif(nrow(df))  # create temporary list of random probablities

  timetoevent <- ifelse(
    randomrisk <= eventprobability,
    as.integer(followuptime * randomrisk),
    followuptime
  )
  # Don't forget the label!
  Hmisc::label(timetoevent) <- label

  return (timetoevent)
}
