library(plotly)
#' Calibration Plot
#' Creates a calibration plot using plotly package. Returns an actual vs 
#' predicted plot. Used for model fit visualization
#'
#' @usage   CalibrationPlot(data, observed, predicted, Group_By, error, title)
#'
#'@param data   a data.frame
#'@param observed variable of observed observation from data.frame, object of 
#'class string
#'@param predicted  variable of predicted observations from data.frame, object 
#'of class string
#'@param group_by_sex  sex variable grouping of interest (optional), object of 
#'class string 
#'@param group_by_1 a primary grouping variable (optional), object of class 
#'string
#'@param group_by_2 a secondary grouping variable (optional), object of class 
#'string
#'@param error percent error for error bars (optional)
#'@param title an overall title for the plot (optional), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#' # load data from Mortality Population Risk Tool from GitHub
#'  data<-read.csv("inst/extdata/MPORT-TABLE1.csv")
#'
#' # check variables for appropriate observed and predicted variable names
#'  head(data, 2L)
#'
#' # view plot - will retrun plot generated in plotly in R studio viewer
#'  CalibrationPlot(data,'observed','predicted','age',Group_By_2='Female',
#'  error=5,title='Female Age')
calibration_plot<- function(data, observed, predicted, group_by_sex=NA, group_by_1=NA, group_by_2=NA, error=0, title=NA) {

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
             x=sub[[observed]],
             y=sub[[predicted]],
             type="scatter",
             marker=list(size = 7),
             mode="markers",
             name="(O, P)",
             showlegend=FALSE,
             error_x=list(type="percent", value=error, color="000000"),
             error_y=list(type="percent", value=error, color="000000")
)
  
max_x=max(sub[[observed]], na.rm=TRUE)
max_y=max(sub[[predicted]], na.rm=TRUE)
max=ifelse(max_x>=max_y, max_x, max_y)
min_x=min(sub[[observed]], na.rm=TRUE)
min_y=min(sub[[predicted]], na.rm=TRUE)
minn=ifelse(min_x>=min_y, min_x, min_y)
min=minn-(minn/8)

trace2<-list(
  x =c(min, max), 
  y =c(min, max), 
  line =list(
    color="000000", 
    dash="dot", 
    width=2
  ), 
  mode="lines", 
  showlegend=FALSE, 
  type="scatter",
  hoverinfo= "skip"
)
  
layout<- list(
  autosize=TRUE,
  title=title,
  hovermode="closest",
  xaxis<-list(
    autorange = TRUE, 
    showspikes = FALSE, 
    title = "Predicted", 
    type = "linear",
    tickformat=".3f"), 
  yaxis<- list(
    autorange = TRUE,
    showline=TRUE,
    showspikes = FALSE, 
    title = "Observed", 
    type = "linear",
    name="predicted",
    tickformat=".3f")
)


p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, error_x=trace1$error_x, error_y=trace1$error_y, marker=trace1$marker, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type)
p <- add_trace(p, x=trace2$x, y=trace2$y, line=trace2$line, mode=trace2$mode, showlegend=trace2$showlegend, type=trace2$type, hoverinfo= trace2$hoverinfo)
p <- layout(p, hovermode=layout$hovermode,autosize=layout$autosize , title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
return(p)
}




