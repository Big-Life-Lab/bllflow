

filter_table_1<- function(data, vars, groupby_sex=NA, groupby_1=NA, groupby_2=NA) {
  if (missing(groupby_sex) & missing(groupby_1) & missing(groupby_2)) {
    x<-subset(data)
  }
  else if (missing(groupby_sex) & missing(groupby_1)) {
    x<-subset(data, data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_1) & missing(groupby_2)) {
    x<-subset(data, data$group_by_sex == groupby_sex)
  }
  else if (missing(groupby_sex) & missing(groupby_2)) {
    x<-subset(data, data$group_by_1== groupby_1)
  }
  else if(missing(groupby_sex)) {
    x<-subset(data, data$group_by_1 == groupby_1 & data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_1)) {
    x<- subset(data, data$group_by_sex == groupby_sex & data$group_by_2 == groupby_2)
  }
  else if (missing(groupby_2)) {
    x<-subset(data, data$group_by_sex == groupby_sex & data$group_by_1 == groupby_1)
  }
  else {
    x<-subset(data, data$group_by_sex == groupby_sex & data$group_by_1 == groupby_1 & data$group_by_2 == groupby_2)
  }
  y<-subset(x, select = vars)
  
  return(y)
}



