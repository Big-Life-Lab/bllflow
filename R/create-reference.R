#' Create Bll Model Object
#' 
#' This object is used to generate the PMML file, for manuscript figures and other uses.
#'
#' @param modelObject The object that is returned when a model is created.
#' @param modelType values = crr, NULL. The class name of the modelObject. "crr" is the class name for the Fine and Grey model. This is currently the only model that is supported.
#' @param tableOne The object returned by createTableOne().
#' @param modelData The data used to generate the model.
#' @param calculateMean default = TRUE. If TRUE and tableOne = missing, then calculate means of variables.
#' @export
CreateBLLModelObject <-
  function(modelData,
           modelObject,
           tableOne = NULL,
           modelType = NULL) {
    # ----Step 1: verify input/create not passed input----
    supportedModelTypes <- c("crr")
    varNames <- attr(modelObject$coef, "names")
    
    if (!class(modelObject) %in% supportedModelTypes) {
      stop("Passed model type is not yet supported. Aborting!")
    }
    if (is.null(tableOne)) {
      tableOne <-
        tableone::CreateTableOne(data = modelData, vars = varNames)
    } else {
      for (varName in varNames) {
        if (!varName %in% tableOne[[pkg.globals$LongTable.MetaData]][[pkg.globals$tableOne.Vars]]) {
          # Issue warning before creating table one
          warning(
            "Passed table one does not contain the vars in the passed model. Creating new TableOne \n"
          )
          # Verify data contains the varNames
          varInData <- varNames %in% colnames(modelData)
          if (all(varInData)) {
            tableOne <-
              tableone::CreateTableOne(data = modelData, vars = varNames)
          } else {
            stop("The modelData does not contain all the variables from the model. Aborting!")
          }
          break()
        }
      }
    }
    
    # ----Step 2: Generate model object ----
    # Obtain the beta coefficient
    betaCoefficient <- modelObject$coef
    attr(betaCoefficient, "names") <- NULL
    allStrataVarMeans <- list()
    retTable <-
      data.frame(betaCoefficient = betaCoefficient, row.names = varNames)
    
    # Obtain the means
    if (!is.null(tableOne$ContTable)) {
      for (strataVar in length(tableOne$ContTable)) {
        allStrataVarMeans[[strataVar]] <-
          tableOne$ContTable[[strataVar]][varNames, pkg.globals$tableOne.Mean]
        attr(allStrataVarMeans[[strataVar]], "names") <- NULL
        retTable[[pkg.globals$tableOne.Mean]] <-
          allStrataVarMeans[[strataVar]]
      }
    } else {
      warning("The tableOne does not contain cont table therefore means were not calculated")
    }
    
    return(retTable)
  }