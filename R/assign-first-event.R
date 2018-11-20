library(Hmisc) 
#' Assign the earliest event, when there are multiple events
#'
#' @description 
#' When there are multiple event dates, finds the earliest data and returns
#' the event and date for that event.

#' @param df (required) Data frame with event propobablity. Each row represents
#'  an observation.
#' @param main_event a list of  dates for main event.
#' @param competing_event a list of dates for competing risk event.
#' @param withdraw_event a list of dates for withdraw or end of study
#' @param followup_time (optinal) Time period for event probablity.
#' @param main_label (optional) event labels.
#' @param competing_label (optional) competiing risk labels.
#' @param withdraw_label (optional) withdraw label.
#' #' @return List with events. List of event dates. 
#' Returned list length = list length of df. Event
#' date =  totalTime (censored), if no event.
#' @keywords make_data make_event_date
#' @examples
#' # load the test data in the bllFlow package
#' \dontrun{
#'  df <- as.data.frame(read.csv("inst/extdata/RESPECT-EOL_validation.csv"))
#'  
#'  # or 
#'  # df <- RESPECT-EOL_validation
#'  
#' # each ovservation as event probablity of df$risk
#'   df$tt_event <- make_event_date(df, df$risk, 1825, units = 'days')
#'   
#'  # add variable labeles
#'    
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
#'   df$ttcensor <- make_event_date(df, .01, 1825)
#' }
#' @export assign_first_event
assign_first_event <- function (df, main_event, competing_event = NA, 
                                withdraw_event = NA, followup_time, 
                                main_label = "main event",
                                competing_label = "competing event",
                                withdraw_label = "withdraw from study")
  {
  
  # which event came first
  event_list <- c(main_event, competing_event, withdraw_event, followup_time)
  censor <- which.min(event_list)
  
  # but check if main event was actually end-of-study
  if (main_event == followup_time) {
      censor = 3
      }

  Hmisc::label(time_to_event) <- label
  units(time_to_event) <- units
  
  # prc_cvd=0.25
  # prc_death=0.7
  # prc_overall=0.15
  # 
  # originaldata$r<-runif(nrow(originaldata))
  # originaldata$rcvd<-runif(nrow(originaldata))
  # originaldata$rdeath<-runif(nrow(originaldata))
  # 
  # originaldata$ttevent<-ifelse(originaldata$r<=prc_overall,as.integer(1825*originaldata$r), 1825)
  # originaldata$censor<-ifelse(originaldata$ttevent==1825,0,
  #                             ifelse(originaldata$rcvd<=prc_cvd,1,
  #                                    ifelse(originaldata$rdeath<=prc_death,2,0)))
  # 
  # 
  # Don't forget the label.
  Hmisc::label(time_to_event) <- label
  units(time_to_event) <- units
  
  return (time_to_event)
}





#' @param probablityWithdraw (optional) Probablity the observation was lost to
#' follow-up prior to end of follow up (followUpTime)
#' @param probablityCompetingEvent (optional) Probablity the observation was a
#' competing event.