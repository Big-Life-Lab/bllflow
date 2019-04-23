pkg.globals <- new.env()
# Variables for variables column presence check
pkg.globals$columnNames.Min <- "min"
pkg.globals$columnNames.Max <- "max"
pkg.globals$columnNames.Outlier <- "outlier"

# Variables for the column names in variable details
pkg.globals$argument.Data <- "data"
pkg.globals$argument.Variables <- "variables"
pkg.globals$argument.VariableDetailsSheet <- "variableDetailsSheet" 
pkg.globals$argument.VariableStart <- "variableStart" 
pkg.globals$argument.VariableStartType <- "variableStartType"
pkg.globals$argument.VariableStartHigh <- "high" 
pkg.globals$argument.VariableStartLow <- "low" 
pkg.globals$argument.CatStartValue <- "value"
pkg.globals$argument.CatStartLabel <- "valueLabelStart"
pkg.globals$argument.VariableStartLabel <- "label"

# Variables for tags in ddi file
pkg.globals$ddiValue.Min <- "min"
pkg.globals$ddiValue.Max <- "max"
pkg.globals$ddiValueName.Cont <- "cont"
pkg.globals$ddiValueName.Cat <- "cat"

# Variables for actual bllFlow objects
pkg.globals$bllFlowContent.Variables <- "variables"
pkg.globals$bllFlowContent.VariableDetails <- "variableDetails"
pkg.globals$bllFlowContent.PopulatedVariableDetails <- "populatedVariableDetails"