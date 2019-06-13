# Recode variables using a look-up table that has the starting variable and 
# end variable. see also: switch(), cut(), dplyr::recode(). car::recode().

#' @ewxample 
data <- read.table(header = T,
                   text = '
                   subject age
                   1   -7.9
                   2   24
                   3   30
                   4   29
                   5   90
                   6   -23
                   ')

library(readr)
other_cats <- read_csv("inst/extdata/other_cats.csv")

# dt, list of observations corresponding to the `start_variable`
# start_variable, the variable label to be transformed. 
# start_variable must be the same as
# other_cats$start_variabble.
# other_cats, look-up table of categories.
# headings include:
# end_variable - final variable name. variable that is genertred from transformation.
# cat_index - category index for new category
# cat_label - label for each category
# cat_label_long - long label for each category
# units - units of category. e.g. years
# lower - low level of category
# upper - hight level of category

# note, categories do not need to be continuous.
# https://en.wikipedia.org/wiki/Interval_(mathematics)
# closed interval (includes the interval), unless preceeding upper interval is
# the same as the lower interval, in which case the interval is open.

recode_with_table <- function (dt,
                               cat_lookup = other_cats,
                               start_variable,
                               end_variable,
                               low = cat_lookup$low,
                               high = cat_lookup$high,
                               invterval = cat_lookup$interval) {
  # check to makes sure dt$start_variable is in other_cats$start_variable
  if(!start_variable %in% cat_lookup$start_variable) 
    stop(sprintf("start_variable not found: %s", start_variable))
  # if(!end_variable %in% other_cats$end_variable) stop("no end_variable identified"))
  
  # if (!start_variable %in% other_cats$start_variable) {
  #   return(print("no start_variable identified") )
  #    }
  # select only rows in other_cats that have the start variable.
  
  find_cat <- function(x) {
    cat_lookup[which(
      cat_lookup$start_variable == start_variable &
        cat_lookup$variable == end_variable &
        low <= x &
        high > x
    ), ]
  }
  
  return(lapply(dt, find_cat))
}

test <-
  recode_with_table(data$age, start_variable = 'age', end_variable = 'age_group_4')
