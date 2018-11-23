library(Hmisc)
#' Assign the earliest event, when there are multiple events
#'
#' @description
#' When there are multiple event dates, finds the earliest data and returns
#' the event and date for that event.
#'
#' @param df (required) Data frame with time-to-event for different events
#' such as main event, competing event or withdraw from study.
#' Each row represents an observation.
#' @param main_event a list of  dates for main event.
#' @param competing_event a list of dates for competing risk event.
#' @param withdraw_event a list of dates for withdraw or end of study
#' @param followup_time (optinal) Time period for event probablity.
#' @param main_label (optional) event labels.
#' @param competing_label (optional) competiing risk labels.
#' @param withdraw_label (optional) withdraw label.
#' #' @return data frame with:
#'      a) censor_events - factor with values
#'              1 = main event
#'              2 = competing event
#'              3 = withdraw from study
#'              4 = end of study
#'      b) censor_time - time to event
#' Returned list length = list length of original data.frame.
#' Event date =  totalTime (censored), if no event.
#' @keywords make_data make_event_date
#' @examples
#' # load the test data in the bllFlow package
#' \dontrun{
#'  df <- as.data.frame(read.csv("inst/extdata/RESPECT-EOL_validation.csv"))
#'
#'  # or
#'  # df <- RESPECT-EOL_validation
#'
#'  # if you don't have the bllFlow package loded.
#'  source(file.path(getwd(), 'R/make-event-date.R'))
#'
#' # create event ata
#'   death    <- make_event_date(df, df$risk, 1825, label = 'death', units = 'days')
#'   competing   <- make_event_date(df, 0.2, 1825, label = 'censor', units = 'days')
#'   withdraw <- make_event_date(df, 0.40, 1825, label = 'withdraw', units = 'days')
#'   death[1] <- 1
#'   withdraw[1] <- NA
#'   withdraw[2] <- 1
#'   competing[3] <-1
#'   
#'
#' # now find with event occurred first and assign the follow-up time to that event.
#' censor1 <- assign_first_event(death, competing_event = NA, withdraw_event = withdraw, followup_time = 1855)
#' censor2 <- assign_first_event(death, competing_event = competing,
#' withdraw_event = withdraw, followup_time = 1825, units = units(death))
#'
#' }
#' @export assign_first_event
assign_first_event <- function (main_event,
                                competing_event = NA,
                                withdraw_event = NA,
                                followup_time,
                                main_label = "main event",
                                competing_label = "competing event",
                                withdraw_label = "withdraw from study",
                                units = NA)
{ withdraw_event[is.na(withdraw_event)] <- followup_time
  competing_event[is.na(competing_event)] <- followup_time
  
  #temperary data.frame
  event_data <- data.frame(main_event, competing_event, withdraw_event)
  print(head(event_data, 10))
  
  # when did the first event happen
  censor_time <- apply(event_data, 1, min, na.rm = TRUE)
  
  # which event came first
  censor_event <- apply(event_data, 1, which.min)
  
  addNA(censor_event) # just in case data
  # add factor labels
  censor_event[censor_time == followup_time] <- 4
  censor_event <- factor(censor_event, levels=c(1,2,3,4), labels=c(main_label, competing_label, withdraw_label, 'end of study'))
  
  # Don't forget the label.
  Hmisc::label(censor_event) <- "censor event"
  Hmisc::label(censor_time) <- "time to event"
  units(censor_time) <- units
  
  censor <- data.frame(censor_event, censor_time)
  
  print(head(censor, 10))
  
  return (censor)
}