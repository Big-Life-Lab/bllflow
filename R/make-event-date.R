library(Hmisc)  # for variable labels only
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
#' @param event_probability (required) Target event probablity (number value
#' from 0 to 1 representing the event probability.e.g. 5-year risk of death is 
#' 0.2 represents 20 \% proabality of dying over 5 years or 1825 days.
#' @param followup_time (required) Time period for event probablity (number, 
#' usually an integer).
#' @param label (optional) Name of event. If missing, default to 'Time to
#' event'.
#' @param units (optional) Time unit for event. e.g. days.
#' #' @return List with event times (integers) Returned list length = 
#' list length of df. Event date =  totalTime (censored), if no event.
#' @keywords make_data assign_first_event
#' @examples
#' # load the test data in the bllFlow package
#' \dontrun{
#' #loaod and example data.frame of observations which includes a calculation of
#'  risk estimate (risk for death at end-of-life).
#'  df <- as.data.frame(read.csv('inst/extdata/RESPECT-EOL_validation.csv'))
#'  # or 
#'  # df <- RESPECT-EOL_validation
#'  df_length <- nrow(df)
#'  
#' # each ovservation as event probablity of df$risk
#'   df$tt_event <- make_event_date(df_length, df$risk, 1825, units = 'days')
#'   
#'  # add variable labeles
#'    df$tt_event <- make_event_date(df_length, df$risk, 1825, 
#'    label = "end-of-life", units = 'days')
#'   
#' # check the label
#'   label(df$tt_event)
#'  
#' # check to see if events have been added
#'   table(df$tt_event)
#'  
#' # check to see if df$tt_event is about the same as df$risk
#'   mean(df$risk)
#'   nrow(df[df$tt_event < 1825,]) / nrow(df)
#' 
#' # make other event data like censoring or competing events
#'   df$ttcensor <- make_event_date(df_length, .01, 1825)
#' }
#' @export make_event_date
make_event_date <- function(num_events,
                            event_probability, 
                            followup_time, 
                            label = "time to event", 
                            units = NA) {
  
  # if !event_label = 'Time to event'
  random_risk <- runif(num_events)  # create temporary list of random probablities
  
  time_to_event <- ifelse(random_risk <= event_probability, 
                          as.integer(followup_time * random_risk), 
                          followup_time)
  # Don't forget the label.
  Hmisc::label(time_to_event) <- label
  units(time_to_event) <- units
  
  return(time_to_event)
}