library(plotly)
#' Observed vs Predicted Plot
#'  Creates an observed versus predicted bar chart with scatter plot overlay using plotly. Used for model fit visualization.
#'
#' @usage   op_plot(data, observed, predicted, title=NA, xlab= NA)
#'
#'@param data	 a data.frame with observed events and predicted risk estimates
#'@param observed	 variable of observed observation from data.frame, object of class string
#'@param predicted	 variable of predicted observations from data.frame, object of class string
#'@param title	 an overall title for the plot (optional), object of class string
#'@param xlab	 a title for the x axis (optional), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#' #upload MPorT table one data set
#' mport<-read.csv("bllFlow/inst/extdata/MPoRT-table-one.csv")
#' 
#' #create character vector of columns needed for visualizations
#' col <- c("observed_risk_1_year", "predicted_risk_1_year", "group_by_sex", "group_by_1", "group_by_2", "group_by_value_label_1")
#' 
#' #create new data frame using filter_table_1 helper function 
#'data<-filter_table_1(mport, col, groupby_sex="Male", groupby_1 ="Drinks per week")
#' 
#'op_plot(data, "observed_risk_1_year", "predicted_risk_1_year", title="Observed vs Predicted for Male Drinks per Week" , xlab="Drinks per Week")



op_plot<-function(data, observed, predicted, title = NA, xlab = NA){

  data$rows<- 1:nrow(data)
  data$rows<-as.numeric(as.character(data$rows))
  data[[predicted]]<- as.numeric(as.character(data[[predicted]]))
  data[[observed]]<- as.numeric(as.character(data[[observed]]))
                                  
  
 if (("group_by_value_label_1" %in% colnames(data)) & ("group_by_value_label_2" %in% colnames(data))) {
   stop("Warning: Cannot plot both sets of labels, refilter data with just one labeling variable")
 }
 else if (("group_by_value_label_1" %in% colnames(data)) & (!"group_by_value_label_2" %in% colnames(data))) {
   data$group_by_value_label_1<-as.character(data$group_by_value_label_1)
   label<-data$group_by_value_label_1
 }
 else if (("group_by_value_label_2" %in% colnames(data)) & (!"group_by_value_label_1" %in% colnames(data))) {
   data$group_by_value_label_2<-as.character(data$group_by_value_label_2)
   label<-data$group_by_value_label_2
 }
 else {
   label<-data$rows
 }

x<-label
    
trace1<-list(
  y=data[[observed]],
  type="bar",
  name="Observed",
  orientation="v",
  showlegend=TRUE
)

trace2<-list(
  y=data[[predicted]],
  type="scatter",
  mode="markers",
  name="Predicted",
  marker=list(symbol= "x"),
  showlegend=TRUE
)

layout<-list(
  autosize=TRUE, 
  hovermode="x", 
  title=title,
  xaxis=list(
    autorange= TRUE, 
    type="category",
    autotick=FALSE,
    title=xlab
  ), 
  yaxis=list(
    autorange=TRUE, 
    type="linear",
    tickformat=".3f"
  )
)

p <-plotly::plot_ly()
p <-plotly::add_trace(p, x=x, y=trace1$y, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, orientation=trace1$orientation)
p <-plotly::add_trace(p, x=x, y=trace2$y, marker=trace2$marker, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, name=trace2$name)
p <-plotly::layout(p, autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, hovermode=layout$hovermode)
return(p)
}



