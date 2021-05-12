# Data verification columns
pkg.globals <- new.env()
pkg.globals$columnNames.Min <- "min"
pkg.globals$columnNames.Max <- "max"
pkg.globals$columnNames.Outlier <- "outlier"
pkg.globals$columnNames.Operations <- "ModuleOperations"
pkg.globals$columnNames.Variable <- "variable"

# Variable Details Sheet Column Names
pkg.globals$argument.Data <- "data"
pkg.globals$argument.Variables <- "variable"
pkg.globals$argument.VariableDetailsSheet <- "variableDetailsSheet"
pkg.globals$argument.VariableStart <- "variableStart"
pkg.globals$argument.VariableStartType <- "variableStartType"
pkg.globals$argument.DatabaseStart <- "databaseStart"
pkg.globals$argument.VariableStartHigh <- "high"
pkg.globals$argument.VariableStartLow <- "low"
pkg.globals$argument.CatStartValue <- "value"
pkg.globals$argument.CatStartLabel <- "valueLabelStart"
pkg.globals$argument.VariableStartLabel <- "label"
pkg.globals$argument.recStart <- "recStart"
pkg.globals$argument.Interval <- "interval"
pkg.globals$argument.recEnd <- "recEnd"
pkg.globals$argument.Notes <- "notes"
pkg.globals$argument.ToType <- "typeEnd"
pkg.globals$argument.Units <- "units"
pkg.globals$argument.VariableLabel <- "labelLong"
pkg.globals$argument.VariableLabelShort <- "label"
pkg.globals$argument.CatLabelLong <- "catLabelLong"
pkg.globals$argument.CatLabel <- "catLabel"
pkg.globals$argument.CatType <- "cat"
pkg.globals$argument.VariableStartHighLow <- "from"
pkg.globals$argument.Role <- "role"
pkg.globals$argument.FromType <- "typeStart"

# DDI object names
pkg.globals$ddiValue.Min <- "min"
pkg.globals$ddiValue.Max <- "max"
pkg.globals$ddiValueName.Cont <- "cont"
pkg.globals$ddiValueName.Cat <- "cat"
pkg.globals$ddiValueName.Categorical <- "Categorical"

# BLLFlow object content
pkg.globals$bllFlowContent.Data <- "working_data"
pkg.globals$bllFlowContent.Variables <- "variables"
pkg.globals$bllFlowContent.VariableDetails <- "variableDetails"
pkg.globals$bllFlowContent.PopulatedVariableDetails <- "populatedVariableDetails"
pkg.globals$bllFlowContent.DDI <- "ddiList"
pkg.globals$bllFlowContent.LongTable <- "longTable"
pkg.globals$bllFlowContent.AdditionalMetaData <- "additionalDDIMetaData"
pkg.globals$bllFlowContent.WorkingData <- "working_data"
pkg.globals$bllFlowContent.PreviousData <- "previous_module_data"
pkg.globals$bllFlowContent.Sequence <- "sequence_number"
pkg.globals$bllFlowContent.Class <- "BLLFlow"

# MSW Column Names
pkg.globals$MSW.Variables.Columns.Variable <- "variable"
pkg.globals$MSW.Variables.Columns.VariableType <- "variableType"
pkg.globals$MSW.Variables.Columns.Label <- "label"
pkg.globals$MSW.Variables.Columns.LabelLong <- "labelLong"
pkg.globals$MSW.Variables.Columns.Units <- "units"

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
pkg.globals$tableOne.Vars <- "vars"

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
pkg.globals$LongTable.ClassName <- "LongTable"
pkg.globals$LongTable.MetaData <- "MetaData"
pkg.globals$LongTable.SmallCells <- "smallCells"
pkg.globals$LongTable.LongTable <- "summaryData"

# Module Column names
pkg.globals$Modules.DefaultOrder <- "DefaultOrder"
pkg.globals$Modules.ModuleID <- "ModuleID"
pkg.globals$Modules.OperationsType <- "OperationsType"

# Module Types
pkg.globals$ModuleTypes.FormulaStep <- "step_with_formula"
pkg.globals$ModuleTypes.DefaultStep <- "step"
pkg.globals$ModuleTypes.Function <- "function"

# WorkingData contents
pkg.globals$WorkingData.ModuleSequenceNumber <- "moduleSequenceNumber"


# FunctionList contents
pkg.globals$FunctionList.Arguments <- "arguments"
pkg.globals$FunctionList.Parameter <- "parameter"
pkg.globals$FunctionList.VariableArguments <- "variablesArguments"

# PMML Export
pkg.globals$ModelExportCSV.variables <- "variables"
pkg.globals$ModelExportCSV.variable_details <- "variable-details"
pkg.globals$ModelExportCSV.model_steps <- "model-steps"

pkg.globals$ModelInternal.file <- "file"

pkg.globals$ModelStepsCSV.filePath <- "filePath"
pkg.globals$ModelStepsCSV.step <- "step"
pkg.globals$ModelStepsCSV.fileType <- "fileType"

pkg.globals$PMML.Node.Application <- "Application"
pkg.globals$PMML.Node.Header <- "Header"
pkg.globals$PMML.Node.DataDictionary <- "DataDictionary"
pkg.globals$PMML.Node.TransformationDictionary <- "TransformationDictionary"
pkg.globals$PMML.Node.DataField <- "DataField"
pkg.globals$PMML.Node.Interval <- "Interval"
pkg.globals$PMML.Node.DerivedField <- "DerivedField"
pkg.globals$PMML.Node.Apply <- "Apply"
pkg.globals$PMML.Node.FieldRef <- "FieldRef"
pkg.globals$PMML.Node.Constant <- "Constant"
pkg.globals$PMML.Node.Array <- "Array"
pkg.globals$PMML.Node.ParameterList <- "ParameterList"
pkg.globals$PMML.Node.FactorList <- "FactorList"
pkg.globals$PMML.Node.CovariateList <- "CovariateList"
pkg.globals$PMML.Node.ParamMatrix <- "ParamMatrix"
pkg.globals$PMML.Node.PPMatrix <- "PPMatrix"
pkg.globals$PMML.Node.Parameter <- "Parameter"
pkg.globals$PMML.Node.PCell <- "PCell"
pkg.globals$PMML.Node.Predictor <- "Predictor"
pkg.globals$PMML.Node.PPCell <- "PPCell"
pkg.globals$PMML.Node.MiningSchema <- "MiningSchema"
pkg.globals$PMML.Node.MiningField <- "MiningField"
pkg.globals$PMML.Node.GeneralRegressionModel <- "GeneralRegressionModel"
pkg.globals$PMML.Node.BaseCumHazardTables <- "BaseCumHazardTables"
pkg.globals$PMML.Node.BaselineCell <- "BaselineCell"
pkg.globals$PMML.Node.PMML <- "PMML"
pkg.globals$PMML.Node.Extension <- "Extension"

pkg.globals$PMML.Extension.attrs.name <- 'name'
pkg.globals$PMML.Extension.attrs.value <- 'value'

pkg.globals$PMML.Extension.names.units <- 'units'
pkg.globals$PMML.Extension.names.variableStartLabel <- 'variableStartLabel'

pkg.globals$variables.Time <- "time"
pkg.globals$variables.splitValue <- ";"
pkg.globals$variables.Value.cont <- "cont"
pkg.globals$variables.Value.cat <- "cat"

pkg.globals$PMML.Node.Attributes.Value.xmlns <- "http://www.dmg.org/PMML-4_4"
pkg.globals$PMML.Node.Attributes.Value.xsi <- "http://www.w3.org/2001/XMLSchema-instance"
pkg.globals$PMML.Node.Attributes.Value.PMML.Version <- "4.4"
pkg.globals$PMML.Node.Attributes.Value.optype.cont <- "continuous"
pkg.globals$PMML.Node.Attributes.Value.optype.cat <- "categorical"
pkg.globals$PMML.Node.Attributes.Value.dataType.float <- "float"
pkg.globals$PMML.Node.Attributes.Value.dataType.string <- "string"
pkg.globals$PMML.Node.Attributes.Value.Type.float <- "float"
pkg.globals$PMML.Node.Attributes.Value.function.if <- "if"
pkg.globals$PMML.Node.Attributes.Value.function.minus <- "-"
pkg.globals$PMML.Node.Attributes.Value.function.equal <- "equal"
pkg.globals$PMML.Node.Attributes.Value.function.rcs <- "rcs"
pkg.globals$PMML.Node.Attributes.Value.function.multiplication <- "*"
pkg.globals$PMML.Node.Attributes.Value.startVar <- "p0"
pkg.globals$PMML.Node.Attributes.Value.Var.incrementVar <- "p"
pkg.globals$PMML.Node.Attributes.Value.label.Intercept <- "Intercept"
pkg.globals$PMML.Node.Attributes.Value.usageType.target <- "target"
pkg.globals$PMML.Node.Attributes.Value.usageType.active <- "active"
pkg.globals$PMML.Node.Attributes.Value.modelType.CoxRegression <- "CoxRegression"
pkg.globals$PMML.Node.Attributes.Value.modelFunction.regression <- "regression"