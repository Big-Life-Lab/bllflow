library(plotly)
#' Calibration Plot
#' Creates a calibration plot using plotly package. Returns an actual vs predicted plot. Used for model fit visualization
#'
#' @usage   CalibrationPlot(data, observed, predicted, title)
#'
#'@param data   a data.frame
#'@param observed variable of observed observation from data.frame, object of class string
#'@param predicted  variable of predicted observations from data.frame, object of class string
#'@param title    an overall title for the plot (use NA for no title), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#'	##Load packages
#' library(plotly)
#'
#' #Load Female risk decile data from Mortality Population Risk csv
#' data <- as.data.frame(read.csv("inst/extdata/RiskDecile.csv")) 
#'
#'	##check variables for appropriate observed and predicted variable names
#' head(data, 2L)
#'
#'	##View plot - will retrun plot generated in plotly in R studio viewer
#' CalibrationPlot(data,'observed','predicted', 'Female Risk Decile')  
CalibrationPlot<- function(data, observed, predicted, title) {

trace1<-list(
             x= data[[observed]],
             y= data[[predicted]],
             type="scatter",
             marker = list(size = 7),
             mode="markers",
             name="(O, P)",
             showlegend = FALSE,
             ##95% error bars
             error_x= list(type="percent", value = 5, color="FF7F0E"),
             error_y= list(type="percent", value= 5, color="FF7F0E")
)
  
##line range for fitted line
max_x=max(data[[observed]], na.rm=TRUE)
max_y=max(data[[predicted]], na.rm=TRUE)
max = ifelse(max_x>=max_y, max_x, max_y )

## y=x line
trace2<-list(
  x = c(0, max), 
  y = c(0, max), 
  line = list(
    color = "rgba(127, 127, 127, 0.72)", 
    dash = "dot", 
    width = 2
  ), 
  mode = "line", 
  showlegend = FALSE, 
  type = "scatter"
)
  
layout<- list(
  autosize=TRUE,
  title = title,
  hovermode= "closest",
  xaxis = list(
    autorange = TRUE, 
    showspikes = FALSE, 
    title = "Predicted", 
    type = "linear",
    ##decimal limit
    tickformat=".3f"), 
  yaxis = list(
    autorange = TRUE, 
    showspikes = FALSE, 
    title = "Observed", 
    type = "linear",
    name="predicted",
    ##decimal limit
    tickformat=".3f")
)


p <- plotly::plot_ly()
## Combine all traces and layout options as defined above  
p <- plotly::add_trace(p, x=trace1$x, y=trace1$y, error_x=trace1$error_x, error_y=trace1$error_y, marker=trace1$marker, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type)
p <- plotly::add_trace(p, x=trace2$x, y=trace2$y, line=trace2$line, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type)
p <- plotly::layout(p, hovermode=layout$hovermode,autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)

return(p)
}