library(plotly)
#' Observed vs Predicted Plot
#'  Creates an observed versus predicted bar chart with scatter plot overlay using plotly. Used for model fit visualization.
#'
#' @usage   op_plot(data, observed, predicted, Group_By, Group_By_2=NA, title=NA, xlab= NA)
#'
#'@param data	 a data.frame with observed events and predicted risk estimates
#'@param observed	 variable of observed observation from data.frame, object of class string
#'@param predicted	 variable of predicted observations from data.frame, object of class string
#'@param group_by_sex sex variable grouping of interest (optional), object of class string 
#'@param group_by_1  a primary grouping variable (optional), object of class string
#'@param group_by_2 a secondary grouping variable (optional), object of class string
#'@param title	 an overall title for the plot (optional), object of class string
#'@param xlab	 a title for the x axis (optional), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#'	##Load packages
#' library(plotly)
#'	##Load data from Mortality Population Risk Tool from GitHub
#'data<-read.csv(curl("https://raw.githubusercontent.com/Big-Life-Lab/bllFlow/data-visualization/inst/extdata/MPORT-TABLE1.csv?token=ApmTWa9Df_Oqmw7mj7EZDPn5LAA9dKI2ks5cCDiLwA%3D%3D"))
#'
#'	##check variables for appropriate observed and predicted variable names
#'head(data, 2L)
#'
#'	##View plot - will retrun plot generated in plotly in R studio viewer
#'CalibrationPlot(data,'observed','predicted','age', Group_By_2 ='female', title='Observed and Predicted for Age in Females' , xlab='Female Age')   

op_plot<-function(data, observed, predicted, group_by_sex=NA, group_by_1=NA, group_by_2=NA , title = NA, xlab = NA){

  if (missing(group_by_sex)&missing(group_by_1)&missing(group_by_2)) {
    sub<-subset(data)
  } 
  else if (missing(group_by_1)& missing(group_by_2)) {
    sub<-subset(data, data$group_by_sex==group_by_sex)
  }
  else if (missing(group_by_2)) {
    sub<-subset(data, data$group_by_sex==group_by_sex & data$group_by==group_by_1)
  }
  else if (missing(group_by_sex) & missing(group_by_2)) {
    sub<-subset(data, data$group_by==group_by_1)
  }
  else if (missing(group_by_sex)) {
    sub<-subset(data, data$group_by==group_by_1 & data$group_by_2==group_by_2)
  }
  else {
    sub<-subset(data, data$group_by_sex==group_by_sex & data$group_by==group_by_1 & data$group_by_2==group_by_2)
  }
  
trace1<-list(
  x=sub$group_by_value_label,
  y=sub[[observed]],
  type="bar",
  name="Observed",
  orientation="v",
  showlegend=TRUE
)

trace2<-list(
  x=sub$group_by_value_label,
  y=sub[[predicted]],
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
p <-plotly::add_trace(p, x=trace1$x, y=trace1$y, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, orientation=trace1$orientation)
p <-plotly::add_trace(p, x=trace2$x, y=trace2$y, marker=trace2$marker, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, name=trace2$name)
p <-plotly::layout(p, autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, hovermode=layout$hovermode)
return(p)
}


