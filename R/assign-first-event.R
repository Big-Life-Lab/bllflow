library(Hmisc)
library(dplyr)
#' Assign the earliest event, when there are multiple events
#'
#' @description
#' Finds the earliest date and returns the event and date for that event. Use 
#' when there are multiple events and dates. Each row represents an observation.
#'  The output of this function creates a variable that is used for survival 
#'  models and predictive algorithms.
#' 
#' Input are lists of event dates for the main event (outcome), competing event
#' (optional) and withdraw event (optional). The maximum follow-up time must be 
#' specified (followup_time).
#' 
#' The lists should be the same length but, if not, the return list is the same 
#' length as the longest input list.
#' @param main_events The main outcome or event. A list of  numbers 
#' (typically integers) that represent a unit of time (typically days). 
#' @param competing_events The competing event (optional). A list of  numbers 
#' (typically integers) that represent a unit of time (typically days).
#' @param withdraw_events The withdraw event (optional). A list of  numbers 
#' (typically integers) that represent a unit of time (typically days).
#' @param followup_time The follow-up time period (optinal). A number (typically 
#' and integer). 'end of study' will be the assigned censor event if any event 
#' (main, competing or withdraw events) has the value equal the followup_time.
#' @param main_label (optional) Label for the main event (default "main event").
#' @param competing_label (optional) Label for the competing risk event (default
#'  "competing event").
#' @param withdraw_label (optional) Label for the withdraw event (default 
#' "withdraw form study")
#' #' @return data frame with two variables:
#'      a) 'censor_event' - factor with levels and labels of the different 
#'      potential first events:
#'              1 = "main event"
#'              2 = "competing event"
#'              3 = "withdraw from study"
#'              4 = "end of study"
#'      b) 'censor_time' - time to event. Number (usually an integer).
#' Returned data.frame length = length of original lists. 
#' Event date =  followup_tme (censored), if no event.
#' @keywords make_data make_event_date
#' @examples
#' \dontrun{
#' #' # load the test data in the bllFlow package
#'   df <- as.data.frame(read.csv("inst/extdata/RESPECT-EOL_validation.csv"))
#'
#' # or
#' #  df <- RESPECT-EOL_validation
#'
#' # if you don't have the bllFlow package loded.
#'  source(file.path(getwd(), 'R/make-event-dates.R'))
#'
#' # create event data. Each new variable is a list of events that is the same
#' # length, nrow(df), as the orginial data, df.
#'   death    <- make_event_date(nrow(df), df$risk, 1825, label = 'death', units = 'days')
#'   competing   <- make_event_date(nrow(df), 0.2, 1825, label = 'censor', units = 'days')
#'   withdraw <- make_event_date(nrow(df), 0.40, 1825, label = 'withdraw', units = 'days')
#'
#' # now find with event occurred first and assign the follow-up time to that event.
#'   censor <- assign_first_event(death, competing_events = competing,
#'     withdraw_events = withdraw, followup_time = 1825, units = units(death))
#' # the return is a data.frame (censor) with two new variables:
#' # censor$censor_event and censor$censor_time
#' summary(censor)
#' 
#' # you can add the new variables to the original data
#' df <- df %>% bind_cols(censor)
#'     
#' }
#' @export assign_first_event
assign_first_event <- function (main_events,
                                competing_events = NA,
                                withdraw_events = NA,
                                followup_time,
                                main_label = "main event",
                                competing_label = "competing event",
                                withdraw_label = "withdraw from study",
                                units = NA) { 
  
  competing_events_to_max <- ifelse(is.na(competing_events), 
                                  followup_time, 
                                  competing_events) 
  withdraw_events_to_max <- ifelse(is.na(withdraw_events), 
                                  followup_time, 
                                  withdraw_events)
   
  #temperary data.frame
  event_data <- data.frame(main_events, 
                           competing_events_to_max, 
                           withdraw_events_to_max) 
  
  # identify the time period when the first event happened
  censor_time <- apply(event_data, 1, min, na.rm = TRUE)
  
  # identify which event happened first. See @return censor_event for how this 
  # variable is labeled. 
  censor_event <- apply(event_data, 1, which.min)

  # check to make sure censor_event was correctly generated. If not correctly 
  # generated, assign value of NA
  addNA(censor_event) 
  
  # add factor labels
  censor_event[censor_time == followup_time] <- 4
  censor_event <- factor(censor_event, levels=c(1,2,3,4), 
                         labels=c(main_label, 
                                  competing_label, 
                                  withdraw_label, 
                                  'end of study'))
  
  # Don't forget the label.
  
  Hmisc::label(censor_event) <- "censor event"
  Hmisc::label(censor_time) <- "time to event"
  units(censor_time) <- units
  
  return (data.frame(censor_event, censor_time))
}