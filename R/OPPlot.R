library(plotly)
#' Observed vs Predicted Plot
#'  Creates an observed versus predicted bar chart with scatter plot overlay using plotly. Used for model fit visualization.
#'
#' @usage   OvPPlot(data, observed, predicted, title, ylab)
#'
#'@param data	 a data.frame with observed events and predicted risk estimates
#'@param observed	 variable of observed observation from data.frame, object of class string
#'@param predicted	 variable of predicted observations from data.frame, object of class string
#'@param title	 an overall title for the plot (optional), object of class string
#'@param ylab	 a title for the y axis (optional), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#'	##Load packages
#' library(plotly)
#' #Load Female risk decile data from Mortality Population Risk csv from GitHub
#' data <- data.frame(read.csv("inst/extdata/RiskDecile.csv")) 
#'
#' #view plot - will return plot generated in plotly in R studio viewer
#' OvPPlot(data, 'observed', 'predicted', 'Observed vs Predicted for Female Risk Decile', 'Risk Decile')  
OvPPlot <- function(data, observed, predicted, title = NA, ylab = NA){

data$NumObs <- 1:(nrow(data))
  
##bar char for observed observations
trace1<- list(
  x= data$NumObs,
  y= data[[observed]],
  type="bar",
  name="Observed",
  orientation="v",
  showlegend=TRUE
)

##scatter plot overlay for predicted observations
trace2<- list(
  x=data$NumObs,
  y=data[[predicted]],
  type="scatter",
  mode= "markers",
  name="Predicted",
  marker=list(symbol= "x"),
  showlegend=TRUE
)

layout <- list(
  autosize = TRUE, 
  hovermode = "x", 
  title = title,
  
  xaxis = list(
    autorange = TRUE, 
    type = "linear",
    autotick=FALSE
  ), 
  yaxis = list(
    autorange = TRUE, 
    type = "linear",
    title = ylab,
    tickformat=".3f"
  )
)

p <- plotly::plot_ly()
p <- plotly::add_trace(p, x=trace1$x, y=trace1$y, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, orientation=trace1$orientation)
p <- plotly::add_trace(p, x=trace2$x, y=trace2$y, marker=trace2$marker, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, name=trace2$name)
p <- plotly::layout(p, autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, hovermode=layout$hovermode)
return(p)
}
