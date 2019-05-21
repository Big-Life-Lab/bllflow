# Data verification columns
pkg.globals <- new.env()
pkg.globals$columnNames.Min <- "min"
pkg.globals$columnNames.Max <- "max"
pkg.globals$columnNames.Outlier <- "outlier"

# Variable Details Sheet Column Names
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

# DDI object names
pkg.globals$ddiValue.Min <- "min"
pkg.globals$ddiValue.Max <- "max"
pkg.globals$ddiValueName.Cont <- "cont"
pkg.globals$ddiValueName.Cat <- "cat"
pkg.globals$ddiValueName.Categorical <- "Categorical" 

# BLLFlow object content
pkg.globals$bllFlowContent.Data <- "data"
pkg.globals$bllFlowContent.Variables <- "variables"
pkg.globals$bllFlowContent.VariableDetails <- "variableDetails"
pkg.globals$bllFlowContent.PopulatedVariableDetails <- "populatedVariableDetails"
pkg.globals$bllFlowContent.DDI <- "ddi"
pkg.globals$bllFlowContent.LongTable <- "longTable"

# MSW Column Names
pkg.globals$MSW.Variables.Columns.Variable <- "variable"
pkg.globals$MSW.Variables.Columns.VariableType <- "variableType"

# Table one variable names
pkg.globals$tableOne.p75 <- "p75"
pkg.globals$tableOne.p25 <- "p25"
pkg.globals$tableOne.Miss <- "miss"
pkg.globals$tableOne.Mean <- "mean"
pkg.globals$tableOne.SD <- "sd"
pkg.globals$tableOne.Freq <- "freq"
pkg.globals$tableOne.Level <- "level"
pkg.globals$tableOne.Percent <- "percent"
pkg.globals$tableOne.StrataVarName <- "strataVarName"
pkg.globals$tableOne.N <- "n"

# Long table column names
pkg.globals$LongTable.VariableCategory <- "variableCategory"
pkg.globals$LongTable.VariableCategoryLabel <- "variableCategoryLabel"
pkg.globals$LongTable.Variable <- "variable"
pkg.globals$LongTable.Prevalence <- "prevalence"
pkg.globals$LongTable.Frequency <- "n"
pkg.globals$LongTable.NMissing <- "nMissing"
pkg.globals$LongTable.Mean <- "mean"
pkg.globals$LongTable.SD <- "sd"
pkg.globals$LongTable.Percentile25 <- "percentile25"
pkg.globals$LongTable.Percentile75 <- "percentile75"
pkg.globals$LongTable.GroupBy <- "groupBy"
pkg.globals$LongTable.GroupByValue <- "groupByValue"
pkg.globals$LongTable.GroupByLabel <- "groupByLabel"
pkg.globals$LongTable.GroupByValueLabel <- "groupByValueLabel"