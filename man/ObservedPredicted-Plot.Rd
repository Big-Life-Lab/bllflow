\name{Observed vs Predicted Plot}
\alias{OvP}
\description{
A function to generate an observed versus predicted bar chart with scatter plot overlay using plotly. Returns an observed (or actual) vs predicted plot. Used for model fit visualization.
}
\usage{
OvP(data, observed, predicted, title, ylab)
}

\arguments{
  \item{dataframe}{
a data.frame     
}
  \item{observed}{
variable of observed observations from data.frame
}
  \item{predicted}{
variable of predicted observations from data.frame
}
  \item{title}{
an overall title for the plot (use NA for no title)
}
  \item{ylab}{
a title for the y axis (use NA for no title)
}
\examples{
library(plotly)
library(curl)

##Load Female risk decile data from Mortality Population Risk csv from GitHub
#data<-read.csv(curl("https://raw.githubusercontent.com/Big-Life-Lab/MPoRTv2/master/Development/Datasets/RiskDecile.csv%5C?token=ApmTWZ2LBGKyfkpPvTHUG9-f_w0D0Nb4ks5b5J1zwA%3D%3D"))


##check variables for appropriate observed and predicted variable names
#head(data, 2L)

##View plot
#OvP(data,'observed','predicted', 'Observed vs Predicted for Female Risk Decile', 'Risk Decile')

##save plot
#myPlot <- OvP(data,'observed','predicted', 'Observed vs Predicted for Female Risk Decile', 'Risk Decile')
 

}

