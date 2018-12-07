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


op_plot<-function(data, observed, predicted, title = NA, xlab = NA){

  
trace1<-list(
  x=data$group_by_value_label,
  y=data[[observed]],
  type="bar",
  name="Observed",
  orientation="v",
  showlegend=TRUE
)

trace2<-list(
  x=data$group_by_value_label,
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
p <-plotly::add_trace(p, x=trace1$x, y=trace1$y, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, orientation=trace1$orientation)
p <-plotly::add_trace(p, x=trace2$x, y=trace2$y, marker=trace2$marker, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, name=trace2$name)
p <-plotly::layout(p, autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, hovermode=layout$hovermode)
return(p)
}


