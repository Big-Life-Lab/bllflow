\name{Calibration Plot}
\alias{CalibrationPlot}
\description{
A function to generate a calibration plot using pltoly. Returns an observed (or actual) vs predicted scatter plot. Used for model fit visualization.
}
\usage{
CalibrationPlot(data, observed, predicted, title)
}

\arguments{
  \item{data}{
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
\examples{
library(plotly)
library(curl)

##Load Female risk decile data from Mortality Population Risk csv from GitHub
#data<-read.csv(curl("https://raw.githubusercontent.com/Big-Life-Lab/MPoRTv2/master/Development/Datasets/RiskDecile.csv%5C?token=ApmTWZ2LBGKyfkpPvTHUG9-f_w0D0Nb4ks5b5J1zwA%3D%3D"))


##check variables for appropriate observed and predicted variable names
#head(data)

##View plot
#CalibrationPlot(data,'observed','predicted', 'Female Risk Decile')

##save plot
#myPlot <- CalibrationPlot(data, 'observed', 'predicted', 'Female Risk Decile')



}

