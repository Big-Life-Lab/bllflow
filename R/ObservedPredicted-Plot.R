#' Creates an observed (or actual) versus predicted bar chart with scatter plot overlay using plotly. Used for model fit visualization.
#'
#' @usage   OvP(data, observed, predicted, title, ylab)
#'
#'@param data	        a data.frame
#'@param observed	variable of observed observation from data.frame
#'@param predicted	variable of predicted observations from data.frame
#'@param title	        an overall title for the plot (use NA for no title)
#'@param ylab	        a title for the y axis (use NA for no title)
#'
#'@author Molly Campbell
#'
#'@examples
#'
#'	##Load packages
#' library(plotly)
#' library(curl)
#'	##Load Female risk decile data from Mortality Population Risk csv from GitHub
#' data<-read.csv(curl("https://raw.githubusercontent.com/Big-Life-Lab/MPoRTv2/master/Development/Datasets/RiskDecile.csv%5C?token=ApmTWZ2LBGKyfkp##PvTHUG9-f_w0D0Nb4ks5b5J1zwA%3D%3D"))
#'
#'	##check variables for appropriate observed and predicted variable names
#' head(data, 2L)
#'
#'	##view plot - will return plot generated in plotly in R studio viewer
#' OvP(data, 'observed', 'predicted', 'Observed vs Predicted for Female Risk Decile', ‘Risk Decile’)  
#'
#'	##save plot
#' myPlot <- OvP(data, 'observed', 'predicted', 'Observed vs Predicted for Female Risk Decile', ‘Risk Decile’)
#'
#'@export




OvP<- function(data, observed, predicted, title, ylab){

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

p <- plot_ly()
  ## Combine trace and layout elements together as defined above
p <- add_trace(p, x=trace1$x, y=trace1$y, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, orientation=trace1$orientation)
p <- add_trace(p, x=trace2$x, y=trace2$y, marker=trace2$marker, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, name=trace2$name)
p <- layout(p, autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, hovermode=layout$hovermode)
return(p)
}
