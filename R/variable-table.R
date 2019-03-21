## generating error during build comment out until debugging
# require(tableone)
# 
# CreateTableOne(data=originaldata)
# 
# respectdid<-originaldata
# respectdid$ran_id<-NULL
# 
# names(respectdid)
# 
# inames<-names(respectdid)[sapply(respectdid, is.integer)]
# 
# inames
# names(respect)
# nnames
# 
# respectdid[inames]<-lapply(respectdid[inames], as.factor)
# 
# table1one<-CreateTableOne(data=respectdid, strata="Sex_cat2_1")
# table1onemat<-print(table1one, noSpaces=TRUE, printToggle=FALSE)
# write.csv(table1onemat, file= "table2csv.csv")
# 

