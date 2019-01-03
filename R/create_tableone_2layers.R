library(tidyr)
library(Hmisc)
library(dplyr) # for data manipulation
library(knitr) # for printing tables

#' FIrst attempt for table 1 creation function
#' Calculates mean of a covariate (T1_covariate) for two layers
#' defined by   GroupByName1 and   GroupByName2
#' missings are expressed in a seperate category.
#'
#'
#'
#' @param datasetname The name of the original dataset that has the covariate that mean/stdev will be calculated for
#' @param CovariateName1  The name of the original variable that stats (mean/stdev etc) will be calculated for
#' @param GroupByName1  variable name for the first layer
#' @param GroupByName2 variable for the second layer of the table
#'
#'
#' Messed up variable has the same name with "dirty" prefix
#'
#' @return dataframe with categories and stats
#' @export
#'
#' @example
#'
#'   messyEducation_cat variable has 8 categories including the missing category
#'   messyDiabetes_bin variable is a binary variable with a separate category for missings
#'   Risk_cont is a continous risk variable
#'   da  is the name of the dataset that has all the variables
#'
#'   table1<-create_tableone_2layers(
#' CovariateName1=da$Risk_cont,
#' GroupByName1=da$Education_cat,
#' GroupByName2=da$Diabetes_bin,
#' datasetname=da)
#'
#' table2<-create_tableone_2layers(
#'  CovariateName1=da$Age_cont,
#'  GroupByName1=da$Education_cat,
#'  GroupByName2=da$Diabetes_bin,
#'  datasetname=da)

#' tableall<-rbind.fill(table1, table2)
#'

create_tableone_2layers <- function(CovariateName1,
                                    GroupByName1,
                                    GroupByName2,
                                    datasetname) {
  # calculate mean of CovariateName1 for every category of GroupByName1 and GroupByName2

  partialtable <- as.data.frame(aggregate(CovariateName1 ~ addNA(GroupByName1) +
    addNA(GroupByName2), datasetname, mean, na.rm = FALSE, na.action = na.exclude))



  # create columns for variable labels

  partialtable$GroupBy1_label <- sub(".*\\$", "", deparse(substitute(GroupByName1)))

  partialtable$GroupBy2_label <- sub(".*\\$", "", deparse(substitute(GroupByName2)))

  covariate1_att <- deparse(substitute(CovariateName1))

  # rename columns
  names(partialtable) <- c("GroupBy1_Value", "GroupBy2_Value", covariate1_att, "GroupBy1_label", "GroupBy2_label")


  # calculate the prevalence of the categories

  da$percent <- 1 / nrow(datasetname)

  prevalence <- aggregate(da$percent ~ addNA(GroupByName1) + addNA(GroupByName2),
    datasetname,
    FUN = sum, na.rm = FALSE, na.action = na.exclude
  )

  names(prevalence)[3] <- "prevalence (%)"

  # calculate the size of each category

  da$one <- 1

  size <- aggregate(da$one ~ addNA(GroupByName1) + addNA(GroupByName2),
    datasetname,
    FUN = sum, na.rm = FALSE, na.action = na.exclude
  )

  names(size)[3] <- "prevalence (n)"

  tableone_part1 <- cbind(partialtable, prevalence[3], size[3])

  return(tableone_part1)
}
