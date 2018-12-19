
#' Makes 'messy' data
#' makes invalid and missing data to a dataset that has been previously 
#' cleaned.
#' 
#' Use cases include:
#'  - teaching how to clean a dataset or impute missing data
#'  - unit testing cleaning and data imputation functons
#'  - unit testing predictive algoirthm scoring engines.
#'
#' @param vartomess The name of the original variable to be messed up
#' @param messy0missing1  value = 0 to introduce messy data, 1 to introduce 
#' missing values
#' @param prc of messy data/missing values to be introduced. 
#' e.g. 0.05 for 5 percent messy/missing
#' 
#' @return messy vartomess variable
#' @export
#'
#' @examples 
#' #'
#' These are more like 'vingettes' TODO: make a vingett and change
#' to simple function examples.
#' 
#' Example1:
#' You would like to introduce extreme values to 50% of the Bin variable in your partialrespect 
#' dataset and save it as messyBin variable
#' 
#' partialrespect$messyBin<-make_messy_data(partialrespect$Bin, messy0missing1=0,prc=0.5)
#' 
#' Example2
#' You would like to introduce missing values to 10% of the Age_rcs1_c variable in your partialrespect 
#' dataset and save it as messyAge_rcs1_c variable
#' 
#' partialrespect$messyAge_rcs1_c<-make_messy_data(partialrespect$Age_rcs1_c, messy0missing1=1, pr=0.1)

make_messy_data <-function (vartomess, 
                        messy0missing1,
<<<<<<< HEAD:R/make_messy_data.R
                        prc
                      )
{ r<-runif(nrow(as.data.frame(vartomess)))

=======
                        prc,
                        outdataset
)
{ #convert string to variable name
  var<-eval(substitute(variablename), datasetname)
  
  # create a dataset with one variable only 
  datatomessup<<-as.data.frame(var)
  

  
>>>>>>> messy-data:R/messy-data.R
  # mess up or change with missing
  
  # if messy0missing1 is 0 then mess it up!!!
  
  if(messy0missing1=='0')
    
    # for messy data, variable value will be added 15*standard deviation or subtracted 6* standard deviation
    # 15 an 6 are random and can be changed
    
  {
    # calculate the standard deviation
    sd_v <<- sd(vartomess)
    
    # calculate the standard deviation
    sd_v <<- sd(var)
    
    # replacement calculation
    hv<<-vartomess+15*sd_v
    lv<<-vartomess-6*sd_v
    
    # replace if random value is less then or equal to the percentage
    # replace with half with lower value, and other half with higher value defined above.
   
    varmessedup <<-ifelse(r<=prc , ifelse(r>= prc/2, lv, hv), vartomess)   
    
   }
  
  # if messy0missing1 is 1 then replace with missing!!
  
 else
  {
    # replace with missing if random value is less then or equal to the percentage
    
    varmessedup<<-ifelse(r<=prc, NA, vartomess)

  }
  # combine original dataset with messy one
<<<<<<< HEAD:R/make_messy_data.R
return(varmessedup)
=======
  assign(deparse(substitute(outdataset)), cbind(datasetname, datamessedup), envir=.GlobalEnv)
>>>>>>> messy-data:R/messy-data.R
  
}





