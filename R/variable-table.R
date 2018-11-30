## generating error during build comment out until debugging

# this program requires tableone package
# load tableone

require(tableone)

#read data in as respectdid (you might need to change the path and dataset name)

 respectdid<-read.csv("I:/GitHUB1/Respect_nov1_2018.csv")

 # The following code will treat all the variables as continuous
#  CreateTableOne(data=respectdid)
# 
 # so instead identify the integer/categorical variables 
# if id variable exits it takes long for this code to run, so delete the id variable from the dataset
 
 respectdid$ran_id<-NULL
# 
# assign all the integer/categorical variable names to inames
 inames<-names(respectdid)[sapply(respectdid, is.integer)]
# 
# define all integer/categorical variables as factors 
 respectdid[inames]<-lapply(respectdid[inames], as.factor)
# 
# Now the CreateTableOne will treat the integer/categorical variables differently  
# n and % will be calculated for them instead of mean and standard deviation
 overall_tableone<-CreateTableOne(data=respectdid)
 
# if you would like to calculate table for different subgroups provide the subgroup variable in strata:
 table1one<-CreateTableOne(data=respectdid, strata="Sex_cat2_1")
 
# table1onemat<-print(table1one, noSpaces=TRUE, printToggle=FALSE)
# write.csv(table1onemat, file= "table2csv.csv")
# 

