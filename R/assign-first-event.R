library(Hmisc)
#' Assign the earliest event, when there are multiple events
#'
#' @description
#' Finds the earliest date and returns the event and date for that event. Use 
#' when there are multiple events and dates. Each row represents an observation.
#' 
#' Input are lists of event dates for the main event (outcome), competing event
#' (optional) and withdraw event (optional). The maximum follow-up time must be 
#' specified (followup_time).
#' 
#' The lists should be the same length but, if not, the return list is the same 
#' length as the longest imput list.
#' @param main_event The main outcome or event. A list of  numbers 
#' (typically integers) that represent a unit of time (typically days). 
#' @param competing_event The competing event (optional). A list of  numbers 
#' (typically integers) that represent a unit of time (typically days).
#' @param withdraw_event The withdraw event (optional). A list of  numbers 
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
#'      a) 'censor_events' - factor with levels and labels:
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
#'  source(file.path(getwd(), 'R/make-event-date.R'))
#'
#' # create event ata
#'   death    <- make_event_date(df, df$risk, 1825, label = 'death', units = 'days')
#'   competing   <- make_event_date(df, 0.2, 1825, label = 'censor', units = 'days')
#'   withdraw <- make_event_date(df, 0.40, 1825, label = 'withdraw', units = 'days')
#'
#' # now find with event occurred first and assign the follow-up time to that event.
#'   censor2 <- assign_first_event(death, competing_event = competing,
#'     withdraw_event = withdraw, followup_time = 1825, units = units(death))
#' }
#' @export assign_first_event
assign_first_event <- function (main_event,
                                competing_event = NA,
                                withdraw_event = NA,
                                followup_time,
                                main_label = "main event",
                                competing_label = "competing event",
                                withdraw_label = "withdraw from study",
                                units = NA) { 
  
  competing_event_to_max <- ifelse(is.na(competing_event), 
                                  followup_time, 
                                  competing_event) 
  withdraw_event_to_max <- ifelse(is.na(withdraw_event), 
                                  followup_time, 
                                  withdraw_event)
   
  #temperary data.frame
  event_data <- data.frame(main_event, 
                           competing_event_to_max, 
                           withdraw_event_to_max) 
  
  # when did the first event happen
  censor_time <- apply(event_data, 1, min, na.rm = TRUE)
  
  # which event came first
  censor_event <- apply(event_data, 1, which.min)
  
  addNA(censor_event) # just in case data
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