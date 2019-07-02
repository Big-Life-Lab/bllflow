#' @title Recode variables with table
#' @name RecWTable
#' 
#' @description \code{RecWTable()} recodes values of variables, where variable selection is based variable names or column position, or on select helpers (see documentation on ...). The rules for recoding are contained within a data.table.
#' 
#' @seealso \code{sjmisc::rec()} for the recoding using the same syntax but without rules in a data.table.
#' 
#' #' @param predicate A predicate function to be applied to the columns. The
#'   variables for which \code{predicate} returns \code{TRUE} are selected.
#' @param rec String with recode pairs of old and new values. See 'Details'
#'   for examples. \code{\link{rec_pattern}} is a convenient function to
#'   create recode strings for grouping variables.
#' @param as.num Logical, if \code{TRUE}, return value will be numeric, not a factor.
#' @param var.label Optional string, to set variable label attribute for the
#'   returned variable (see vignette \href{https://cran.r-project.org/package=sjlabelled/vignettes/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}).
#'   If \code{NULL} (default), variable label attribute of \code{x} will
#'   be used (if present). If empty, variable label attributes will be removed.
#' @param val.labels Optional character vector, to set value label attributes
#'   of recoded variable (see vignette \href{https://cran.r-project.org/package=sjlabelled/vignettes/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}).
#'   If \code{NULL} (default), no value labels will be set. Value labels
#'   can also be directly defined in the \code{rec}-syntax, see
#'   'Details'.
#' @param append Logical, if \code{TRUE} (the default) and \code{x} is a data frame,
#'   \code{x} including the new variables as additional columns is returned;
#'   if \code{FALSE}, only the new variables are returned.


#' @details The  \code{variableDetails} data.table has following syntax:
#'    \describe{
#'    \item{}
#' 

RecWTable(data, ..., variableDetails, label = TRUE, datasetName, else = NA, append = TRUE, log = TRUE, printLog = TRUE, printNote = TRUE, printNote = TRUE, predicate)

data: Data that contains starting variables for recoding. A vector or data frame.

...: The unquoted name(s) of variable(s) that are created during recoding, as defined in `variableDetails`. The variable(s) must exist in `variableDetails`. 

variableDetails: The data.table that contains the variables and corresponding rules and labels for recoding.

label: (@rusty, not sure if this needs to be a quoted character) If TRUE, will set variable label attribute for the returned variable from 'variableDetails$label' (if present).  If unquoted `name` will set variable label attribute from the corresponding string from from `variableDetails$name`. For example, use `shortLabel` to set label from `variableDetails$shortLabel`.  If empty, variable label attributes will be removed.

datasetName: The name of the data for starting variables. This is an optional (but recommended) check to ensure if startVariables correspond to correct data.

else: assigns values to all other values (aka out-of-range values) that cannot be recoded according to rules in `variableDetails`. `else` can also be defined as a rule in `variableDetails`. Default = NA, out-of-range values are assigned `NA`. If `copy`, then the out-of-range values are not recoded (maintain their original value).

append: Logical, if TRUE (the default) and `data` is a data frame, `data` including the new variables as additional columns is returned; if FALSE, only the new variables are returned.

log: Logical, if TRUE, creates log of recoding. default = TRUE.

printLog: Logical, if TRUE, prints summary of recoding to the console.

printNote: Logical, if TRUE, prints to console the contents of `variableDetails$notes` for variable. 

(@rusty, I'm not sure if we need `predicate`` in our transformation functions. It is in rjmisc and I am including so that we could our functionality with sjmisc.)

predicate: A predicate function to be applied to the columns. The variables for which predicate returns TRUE are selected.
