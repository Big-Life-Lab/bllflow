# Original commit from Meltem. Refacotored as function in makeEvent date
library(dplyr)
library(tidyr)

#### generating errors during build --- comment out until time to debug ----

# df <-
#   as.data.frame(read.csv("inst/extdata/RESPECT-EOL_validation.csv"))
# 
# 
# 
# prc_cvd=0.25
# prc_death=0.7
# prc_overall=0.15
# 
# originaldata$r<-runif(nrow(originaldata))
# originaldata$rcvd<-runif(nrow(originaldata))
# originaldata$rdeath<-runif(nrow(originaldata))
# 
# originaldata$ttevent<-ifelse(originaldata$r<=prc_overall,as.integer(1825*originaldata$r), 1825)
# originaldata$censor<-ifelse(originaldata$ttevent==1825,0,
#                             ifelse(originaldata$rcvd<=prc_cvd,1,
#                                    ifelse(originaldata$rdeath<=prc_death,2,0)))
# 
# 
# table(originaldata$censor)
# 
# 
# 
# 
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


