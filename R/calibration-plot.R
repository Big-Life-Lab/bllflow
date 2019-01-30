library(plotly)
#' Calibration Plot
#' Creates a calibration plot using plotly package. Returns an actual vs 
#' predicted plot. Used for model fit visualization
#'
#' @usage   calibration_plot(data, observed, predicted, error=0, title=NA)
#'
#'@param data   a data.frame
#'@param observed variable of observed observation from data.frame, object of 
#'class string
#'@param predicted  variable of predicted observations from data.frame, object 
#'of class string
#'@param error percent error for error bars (optional)
#'@param title an overall title for the plot (optional), object of class string
#'
#'@author Molly Campbell
#'
#'@examples
#' #upload MPorT table one data set
#' mport <- read.csv("https://raw.githubusercontent.com/Big-Life-Lab/bllFlow/data-visualization/inst/extdata/MPoRT-table-one.csv?token=ApmTWVvRI4hGeBRlPLR6R3P6J73Po5Paks5cWzfswA%3D%3D", 
#'fileEncoding="UTF-8-BOM")
#' 
#' #create character vector of columns needed for visualizations
#' col <- c("observed_risk_1_year", "predicted_risk_1_year", "group_by_sex", "group_by_1", "group_by_2", "group_by_value_label_1")
#' 
#' #create new data frame using filter_table_1 helper function 
#'data<-filter_table_1(mport, col, groupby_sex="Female", groupby_1="age")
#'
#'calibration_plot(data, "observed_risk_1_year", "predicted_risk_1_year", title="Observed vs Predicted for Female age")



calibration_plot<- function(data, observed, predicted, error=0, title=NA) {
  
  
  data[[predicted]]<- as.numeric(as.character(data[[predicted]]))
  data[[observed]]<- as.numeric(as.character(data[[observed]]))  
trace1<-list(
             x=data[[observed]],
             y=data[[predicted]],
             type="scatter",
             marker=list(size = 7),
             mode="markers",
             name="(O, P)",
             showlegend=FALSE,
             error_x=list(type="percent", value=error, color="000000"),
             error_y=list(type="percent", value=error, color="000000")
)
  
max_x=max(data[[observed]], na.rm=TRUE)
max_y=max(data[[predicted]], na.rm=TRUE)
max=ifelse(max_x>=max_y, max_x, max_y)
min_x=min(data[[observed]], na.rm=TRUE)
min_y=min(data[[predicted]], na.rm=TRUE)
min_xy=ifelse(min_x>=min_y, min_x, min_y)
min=min_xy-(min_xy/8)

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


