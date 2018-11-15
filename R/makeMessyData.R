library(tidyr)

#' Makes 'messy' data
#' makes invalid and missing data to a dataset that has been previously 
#' cleaned.
#' 
#' Use cases include:
#'  - teaching how to clean a dataset or impute missing data
#'  - unit testing cleaning and data imputation functons
#'  - unit testing predictive algoirthm scoring engines.
#'
#' @param datasetname The name of the original dataset that has the variable 
#' to be messed up
#' @param variablename The name of the original variable to be messed up
#' @param messy0missing1  value = 0 to introduce messy data, 1 to introduce 
#' missing values
#' @param prc of messy data/missing values to be introduced. 
#' e.g. 0.05 for 5 percent messy/missing
#' @param outdataset Dataset to have original and messed up data. 
#' Messed up variable has the same name with "dirty" prefix
#'
#' @return dataframe with 'messy' data added
#' @export
#'
#' @examples 
#' # These are more like 'vingettes' TODO: make a vingett and change
#' # to simple function examples.
#' #originaldata<-as.data.frame(read.csv('messyd.csv'))
#' #dirtyData(originaldata,BMI, 1, 0.05, dirtyData)
#' 
#' #sink("mess")
#' #dirtydata
#'  #sink()
#' 
#' #dirtyData(originaldata,BMI, 0, 0.05, missiongData)
#' #sink("miss")
#' #missingdata
#' #sink()
# dirtyData <-function (datasetname,  
#                         variablename, 
#                         messy0missing1,
#                         prc,
#                         outdataset
# )
# { #convert string to variable name
#   var<-eval(substitute(variablename), datasetname)
#   
#   # create a dataset with one variable only 
#   datatomessup<<-as.data.frame(var)
#   
#   # calculate the standard deviation
#   sd_v <<- sd(var)
#   
#   # mess up or change with missing
#   
#   # if messy0missing1 is 0 then mess it up!!!
#   if(messy0missing1=='0')
#     
#     # for messy data, variable value will be added 15*standard deviation or subtracted 6* standard deviation
#     # 15 an 6 are random and can be changed
#     
#   {
#     
#     # replacement calculation
#     hv<<-datatomessup$var+15*sd_v
#     lv<<-datatomessup$var-6*sd_v
#     
#     # replace if random value is less then or equal to the percentage
#     # replace with half with lower value, and other half with higher value defined above.
#     
#     datamessedup <<- mutate(datatomessup, r=runif(nrow(datatomessup)), 
#                             var=ifelse(r<=prc , ifelse(r>= prc/2, lv, hv), var))   
#     
#     # clead up the dataset
#     datamessedup$r<<-NULL
#     datamessedup$hv<<-NULL
#     datamessedup$lv<<-NULL
#     
#   }
#   
#   # if messy0missing1 is 1 then replace with missing!!
#   
#   else 
#   {
#     # replace with missing if random value is less then or equal to the percentage
#     
#     datamessedup<<-mutate(datatomessup,r=runif(nrow(datatomessup)), 
#                           var=ifelse(r<=prc, NA, var))
#     #clean up the dataset
#     
#     datamessedup$r<<-NULL
#     
#   }
#   #add "Dirty" to variable name as a prefix
#   
#   colnames(datamessedup)<- paste("Dirty", deparse(substitute(variablename)), sep="")
#   
#   
#   # combine original dataset with messy one
#   assign(deparse(substitute(outdataset)), cbind(originaldata, datamessedup), envir=.GlobalEnv)
#   
# }
