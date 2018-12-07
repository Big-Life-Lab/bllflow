#' Filter_table_1
#' Helper function to filter table 1 data to fit specifc row and column grouping for subsequent data visualizations (ie. calibration plots)
#'
#'@usage   filter_table_1(data, vars, groupby_sex=NA, groupby_1=NA, groupby_2=NA)
#'
#'@param data   a data frame
#'@param vars varaible(s) to keep in new subset. Given as a character vector
#'@param groupby_sex  (Optional) grouping observation of interest from gourp_by_sex variable sex grouping variable (ie "Male" or "Female"). Only rows where the condition evaluates to TRUE are kept 
#'@param groupby_1 (Optional) grouping observation of interest from group_by_1 variable. Only rows where the condition evaluates to TRUE are kept
#'@param groupby_2 (Optional) grouping pbservation of interest from group_by_2 variable. Only rows where the condition evaluates to TRUE are kept
#'
#'@author Molly Campbell
#'
#'@examples
#' Upload MPorT table one data set
#' mport<-read.csv("bllFlow/inst/extdata/MPoRT-table-one.csv")
#' 
#' #create character vector of columns needed for visualizations
#' col <- c("observed_risk_1_year", "predicted_risk_1_year", "group_by_sex", "group_by_1", "group_by_2", "group_by_value_label_1")
#'
#'filter_table_1(mport, col, groupby_sex="Male", groupby_1 ="smoking", groupby_2="packyears")
#'
#'filter_table_1(mport, col, groupby_sex="Female", groupby_1="age")
#'
#'filter_table_1(mport, col, groupby_1="age")

filter_table_1<- function(data, vars, groupby_sex=NA, groupby_1=NA, groupby_2=NA) {
  if (missing(groupby_sex) & missing(groupby_1) & missing(groupby_2)) {
    x<-subset(data)
  }
  else if (missing(groupby_sex) & missing(groupby_1)) {
    x<-subset(data, data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_1) & missing(groupby_2)) {
    x<-subset(data, data$group_by_sex == groupby_sex)
  }
  else if (missing(groupby_sex) & missing(groupby_2)) {
    x<-subset(data, data$group_by_1== groupby_1)
  }
  else if(missing(groupby_sex)) {
    x<-subset(data, data$group_by_1 == groupby_1 & data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_1)) {
    x<- subset(data, data$group_by_sex == groupby_sex & data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_2)) {
    x<-subset(data, data$group_by_sex == groupby_sex & data$group_by_1 == groupby_1)
  }
  else {
    x<-subset(data, data$group_by_sex == groupby_sex & data$group_by_1 == groupby_1 & data$group_by_2 == groupby_2)
  }
  y<-subset(x, select = vars)
  
  return(y)
}



