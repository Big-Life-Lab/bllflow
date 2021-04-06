#' Converts the CSv files that are part of a model export into PMML
#'
#' @param model_export_file_path string The absolute or relative path to the
#' model export file. If relative, the path should relative to the working
#' directory of the project
#' @param database_name string The database from which to take the starting variables
#' for the model
#'
#' @return An object created by the XML::xmlNode function that represents the
#' PMML XML
#'
#' @export
#'
#' @examples
convert_model_export_to_pmml <-
  function(model_export_file_path, database_name) {
    model_export <-
      read.csv(model_export_file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    
    variables_path <-
      model_export[model_export$fileType == "variables", "filePath"]
    variables_path <-
      file.path(dirname(model_export_file_path), variables_path)
    
    variable_details_path <-
      model_export[model_export$fileType == "variable-details", "filePath"]
    variable_details_path <-
      file.path(dirname(model_export_file_path), variable_details_path)
    
    model_steps_path <-
      model_export[model_export$fileType == "model-steps", "filePath"]
    model_steps_path <-
      file.path(dirname(model_export_file_path), model_steps_path)
    
    # Read in variables and variable-details saving them to appropriate variables
    variables <-
      read.csv(variables_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    variable_details <-
      read.csv(variable_details_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    
    
    # Read in model-steps and creating a list for each row of model-steps
    model_steps <-
      read.csv(model_steps_path, fileEncoding = "UTF-8-BOM",  stringsAsFactors = FALSE)
    detected_steps <- unique(model_steps[["step"]])
    step_list <- list()
    
    for (working_step in detected_steps) {
      tmp_rows <-
        model_steps[model_steps[["step"]] == working_step, c("fileType", "filePath")]
      step_list[[working_step]] <-
        list(fileType = tmp_rows[["fileType"]], filePath = tmp_rows[["filePath"]])
    }
    
    # Loop over above list and read in each file specified saving it in a list containing the step name and fileType as well as the content of the file
    if (length(step_list) >= 1) {
      for (single_step in 1:length(step_list)) {
        # Check for sub steps
        if (length(step_list[[single_step]][[1]]) > 1) {
          for (sub_step in 1:length(step_list[[single_step]][[1]])) {
            tmp_file_path <-
              file.path(dirname(model_export_file_path), step_list[[single_step]][["filePath"]][[sub_step]])
            tmp_file <-
              read.csv(tmp_file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
            step_list[[single_step]][["file"]][[sub_step]] <- tmp_file
          }
        } else{
          tmp_file_path <-
            file.path(dirname(model_export_file_path), step_list[[single_step]][["filePath"]])
          tmp_file <-
            read.csv(tmp_file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
          step_list[[single_step]][["file"]] <- tmp_file
        }
        
      }
    }
    # Create empty PMML doc using variables from strings.R to fill in name and version of the package
    doc <-
      XML::xmlNode(
        "PMML",
        namespaceDefinitions = c("http://www.dmg.org/PMML-4_4", xsi = "http://www.w3.org/2001/XMLSchema-instance"),
        attrs = c(version = "4.4")
      )
    header <- XML::xmlNode("Header")
    header <-
      XML::addChildren(header, XML::xmlNode("Application", attrs = c(name = "bllflow", version = "0.1.0")))
    
    
    # Using recodeflow::recode_to_pmml append the empty PMML with DataDictionary and TransformationDictionary
    recodeflow_pmml <-
      recodeflow::recode_to_pmml(
        var_details_sheet = variable_details,
        vars_sheet = variables,
        db_name = database_name
      )
    doc <- XML::addChildren(doc, header, recodeflow_pmml[["DataDictionary"]], recodeflow_pmml[["TransformationDictionary"]])
    working_pmml <- doc
    
    # Calculate max_time from variable_details recTo column
    max_time <-
      max(as.character(variable_details[variable_details[[pkg.globals$argument.Variables]] ==
                                          "time", pkg.globals$argument.CatValue]))
    # Create a vector containing all variables from variableStart matching database name
    all_start_vars <-
      unique(variable_details[grepl(database_name, variable_details[[pkg.globals$argument.DatabaseStart]]), pkg.globals$argument.Variables])
    
    # Loop over the list elements from model-steps
    for (step_name in names(step_list)) {
      working_step <- step_list[[step_name]]
      if (length(working_step[["fileType"]]) > 1) {
        for (sub_step_index in 1:length(working_step[["fileType"]])) {
          tmp_file_type <- working_step[["fileType"]][[sub_step_index]]
          if (is.na(tmp_file_type)) {
            tmp_file_type <- step_name
          }
          tmp_file <- working_step[["file"]][[sub_step_index]]
          column_list <- create_list_from_columns(tmp_file)
          passed_arguments <-
            append(column_list, list(PMML = working_pmml))
          working_pmml <-  switch(
            tmp_file_type,
            'dummy' = {
              do.call(create_dummy_nodes, passed_arguments)
            },
            'center' = {
              do.call(create_center_nodes, passed_arguments)
            },
            'rcs' = {
              do.call(create_rcs_nodes, passed_arguments)
            },
            'interaction' = {
              do.call(create_interaction_nodes, passed_arguments)
            },
            'beta-coefficients' = {
              passed_arguments[["start_variables"]] <- all_start_vars
              do.call(create_beta_coefficient_nodes, passed_arguments)
            },
            'baseline-hazards' = {
              passed_arguments[["max_time"]] <- max_time
              do.call(create_baseline_hazards_nodes, passed_arguments)
            }
          )
        }
      } else{
        tmp_file_type <- working_step[["fileType"]]
        if (is.na(tmp_file_type)) {
          tmp_file_type <- step_name
        }
        tmp_file <- working_step[["file"]]
        column_list <- create_list_from_columns(tmp_file)
        passed_arguments <-
          append(column_list, list(PMML = working_pmml))
        working_pmml <-  switch(
          tmp_file_type,
          'dummy' = {
            do.call(create_dummy_nodes, passed_arguments)
          },
          'center' = {
            do.call(create_center_nodes, passed_arguments)
          },
          'rcs' = {
            do.call(create_rcs_nodes, passed_arguments)
          },
          'interaction' = {
            do.call(create_interaction_nodes, passed_arguments)
          },
          'beta-coefficients' = {
            passed_arguments[["start_variables"]] <- all_start_vars
            do.call(create_beta_coefficient_nodes, passed_arguments)
          },
          'baseline-hazards' = {
            passed_arguments[["max_time"]] <- max_time
            do.call(create_baseline_hazards_nodes, passed_arguments)
          }
        )
      }
    }
    # Return the PMML
    return(working_pmml)
  }



#' Create Dummy Nodes
#'
#' Creates dummy nodes given passed parameters. The passed params must all be of same length
#'
#' @param origVariable a vector representing the original variable name
#' @param catValue a vector representing the original variable cat value
#' @param dummyVariable a vector representing the new dummy variable name
#' @param PMML to append to
#'
#' @return PMML containing the newly added DerivedFields
create_dummy_nodes <-
  function(origVariable,
           catValue,
           dummyVariable,
           PMML) {
    # Verify matching length of all passed vectors
    if (!verify_length(c(origVariable, catValue, dummyVariable))) {
      stop("The length of passed params for dummy nodes does not match")
    }
    
    # Loop over the length of the vectors
    for (vector_index in 1:length(origVariable)) {
      # For each iteration of the loop create a DerivedField node
      # The name attribute is set using dummy_variable value
      # the optype attribute is set to categorical
      # The dataType attribute is set to integer
      tmp_main_node <-
        XML::xmlNode(
          "DerivedField",
          attrs = c(
            name = dummyVariable[[vector_index]],
            optype = "categorical",
            dataType = "integer"
          )
        )
      # Create an Apply node with function attribute set to if
      tmp_apply_node <-
        XML::xmlNode("Apply", attrs = c('function' = "if"))
      # To the Apply node add children nodes of FieldRef and Constant
      # The FieldRef field attibute is populated using orig_variable
      # The Constant dataType attibute is set to float and the value is populated from cat_value
      tmp_field_node <-
        XML::xmlNode("FieldRef", attrs = c(field = origVariable[[vector_index]]))
      tmp_const_apply_node <-
        XML::xmlNode("Constant", attrs = c(dataType = "float"), catValue[[vector_index]])
      # Add the Apply node and two constant Constant nodes as children to the DerivedField node
      tmp_apply_node <-
        XML::addChildren(tmp_apply_node, tmp_field_node, tmp_const_apply_node)
      tmp_const_0 <-
        XML::xmlNode("Constant", attrs = c(dataType = "float"), 0)
      tmp_const_1 <-
        XML::xmlNode("Constant", attrs = c(dataType = "float"), 1)
      # The 2 constant Constant nodes have dataType of float and values of 0 and 1
      tmp_main_node <-
        XML::addChildren(tmp_main_node, tmp_apply_node, tmp_const_1, tmp_const_0)
      # Add the DerivedField to the return PMML
      PMML[["TransformationDictionary"]] <-
        XML::addChildren(PMML[["TransformationDictionary"]], tmp_main_node)
    }
    return(PMML)
  }

#' Create Center Nodes
#'
#' Creates center nodes according to passed params
#'
#' @param origVariable the original variable name
#' @param centerValue value to center on
#' @param centeredVariable name of the centered variable
#' @param centeredVariableType type of the new centered variable
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached centered nodes
create_center_nodes <-
  function(origVariable,
           centerValue,
           centeredVariable,
           centeredVariableType,
           PMML) {
    # Verify matching length of vectors
    if (!verify_length(c(
      origVariable,
      centerValue,
      centeredVariable,
      centeredVariableType
    ))) {
      stop("The length of passed params for center nodes does not match")
    }
    # Loop over the lenght of the vectors
    for (vector_index in 1:length(origVariable)) {
      # For each iteration create a DerivedField node
      # The name attribute comes from centered_variable
      # the optype is determined by centered_variable_type cont = continues, cat = categorical
      # The dataType is determined by centered_variable_type cont = float, cat = string
      optype <- ""
      data_type <- ""
      switch (centeredVariableType[[vector_index]],
              "cont" = {
                optype <- "continues"
                data_type <- "float"
              },
              "cat" = {
                optype = "categorical"
                data_type <- "string"
              })
      tmp_node <-
        XML::xmlNode(
          "DerivedField",
          attrs = c(
            name = centeredVariable[[vector_index]],
            optype = optype,
            dataType = data_type
          )
        )
      # Create a child Apply node with function attribute set to -
      # Create a child FieldRef node for Apply node with the field attribute populated by orig_variable
      # Create a child Constant node for Apply node with dataType attribute set to float and its value set by center_value
      tmp_apply_node <-
        XML::xmlNode("Apply", attrs = c('function' = "-"))
      tmp_field_node <-
        XML::xmlNode("FieldRef", attrs = c(field = origVariable[[vector_index]]))
      tmp_constant_node <-
        XML::xmlNode("Constant", attrs = c(dataType = "float"), centerValue[[vector_index]])
      tmp_apply_node <-
        XML::addChildren(tmp_apply_node, tmp_field_node, tmp_constant_node)
      # Add the DerivedField node to the TransformationDictionary of the passed PMML
      tmp_node <- XML::addChildren(tmp_node, tmp_apply_node)
      PMML[["TransformationDictionary"]] <-
        XML::addChildren(PMML[["TransformationDictionary"]], tmp_node)
    }
    # Return the PMML at the end of the loop
    return(PMML)
  }

#' Create RCS nodes
#'
#' Creates RCS nodes in the TransformationDictionary
#'
#' @param variable list of original variables
#' @param rcsVariables list containing nested lists of rcs_variables for each original variable
#' @param knots list containing the knots
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached rcs nodes
create_rcs_nodes <- function(variable, rcsVariables, knots, PMML) {
  # Verify matching length
  if (!verify_length(c(variable,
                       rcsVariables,
                       knots))) {
    stop("The length of passed params for rcs nodes does not match")
  }
  # Loop over the length of the passed vectors
  for (vector_index in 1:length(variable)) {
    variable_list <- strsplit(rcsVariables[[vector_index]], ";")[[1]]
    # Create the first rcs DerivedField node
    # Create a temporary Constant Array Node with n set to 5, type set to float, and values from knots
    tmp_array_node <-
      XML::xmlNode("Array", attrs = c(n = 5, type = "float"), knots[[vector_index]])
    # The name attribute is set to the first element of the current index of rcs_variables
    # optype attribute is set to continuous and dataType attribute is set to float
    # Create a child Apply node set its attribute function to equal
    # Add child FieldRef node to Apply with attribute field set to variable
    # Add the DerivedField as a child node to TransformationDictionary
    tmp_first_node <-
      XML::xmlNode(
        "DerivedField",
        attrs = c(
          name = variable_list[[1]],
          optype = "continues",
          dataType = "float"
        )
      )
    tmp_first_apply_node <-
      XML::xmlNode("Apply", attrs = c('function' = "equal"))
    tmp_first_field_node <-
      XML::xmlNode("FieldRef", attrs = c(field = variable[[vector_index]]))
    tmp_first_node <-
      XML::addChildren(tmp_first_node,
                       XML::addChildren(tmp_first_apply_node, tmp_first_field_node))
    PMML[["TransformationDictionary"]] <-
      XML::addChildren(PMML[["TransformationDictionary"]], tmp_first_node)
    
    # Create nested loop of rcs_variables current index nested list starting at +1 from start of length
    for (rcs_variable_index in 2:length(variable_list)) {
      # Create DerivedField node with attribute name set to rcs_loop_index inside the nested rcs_variables list
      # optype is set to continues and dataType is set to float
      tmp_node <-
        XML::xmlNode(
          "DerivedField",
          attrs = c(
            name = variable_list[[rcs_variable_index]],
            optype = "continues",
            dataType = "float"
          )
        )
      # Add child Apply node with function node of rcs
      # To the Apply node add fieldRef child node with field set to rcs_loop_index-1 inside the nested rcs_variables list
      # To the Apply node add Constant child node with datatType set to float and value of rcs_loop_index
      tmp_apply_node <-
        XML::xmlNode("Apply", attrs = c('function' = "rcs"))
      tmp_field_node <-
        XML::xmlNode("FieldRef", attrs = c(field = variable_list[[rcs_variable_index -
                                                                    1]]))
      tmp_const <-
        XML::xmlNode("Constant",
                     attrs = c(dataType = "float"),
                     rcs_variable_index)
      # To the Apply node add the temporary constant Array node created in the parent loop
      tmp_apply_node <-
        XML::addChildren(tmp_apply_node,
                         tmp_field_node,
                         tmp_const,
                         tmp_array_node)
      tmp_node <- XML::addChildren(tmp_node, tmp_apply_node)
      # Add the DerivedField as a child node to TransformationDictionary
      PMML[["TransformationDictionary"]] <-
        XML::addChildren(PMML[["TransformationDictionary"]], tmp_node)
    }
  }
  # Return the PMML
  return(PMML)
}

#' Create Interaction Nodes
#'
#' Creates interaction nodes in the TransformationDictionary
#'
#' @param interactionVariable list of interaction_variable names
#' @param interactionVariableType list of interaction_variable types
#' @param interactingVariables list of interacting variable names
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached interaction nodes
create_interaction_nodes <-
  function(interactionVariable,
           interactionVariableType,
           interactingVariables,
           PMML) {
    # Verify matching length
    if (!verify_length(c(
      interactionVariable,
      interactionVariableType,
      interactingVariables
    ))) {
      stop("The length of passed params for interaction nodes does not match")
    }
    # Loop over passed vectors
    for (vector_index in 1:length(interactionVariable)) {
      variable_list <- strsplit(interactingVariables[[vector_index]], ";")[[1]]
      # Create DerivedField node with name attribute set by interaction_variable column
      # optype attribute is determined based on interaction_variable_type cont= continues, cat = categorical
      # dataType attribute is determined based on interaction_variable_type cont= float, cat = string
      optype <- ""
      data_type <- ""
      switch (interactionVariableType[[vector_index]],
              "cont" = {
                optype <- "continues"
                data_type <- "float"
              },
              "cat" = {
                optype = "categorical"
                data_type <- "string"
              })
      tmp_node <-
        XML::xmlNode(
          "DerivedField",
          attrs = c(
            name = interactionVariable[[vector_index]],
            optype = optype,
            dataType = data_type
          )
        )
      # Create Apply child node with function attribute set to *
      tmp_apply_node <-
        XML::xmlNode("Apply", attrs = c('function' = "*"))
      
      # Add length(interacting_variables nested lins) FieldRef child nodes to Apply node with field attribute set by the nested interacting_variables list
      for (interacting_variable in variable_list) {
        tmp_apply_node <-
          XML::addChildren(tmp_apply_node, XML::xmlNode("FieldRef", attrs = c(field = interacting_variable)))
      }
      
      tmp_node <- XML::addChildren(tmp_node, tmp_apply_node)
      # Add the DerivedField as a child node to the PMML TransformationDictionary
      PMML[["TransformationDictionary"]] <-
        XML::addChildren(PMML[["TransformationDictionary"]], tmp_node)
      
    }
    # Return PMML
    return(PMML)
    
  }

#' Create Beta Coefficient Nodes
#'
#' Creates nodes correlating to the beta_coefficient parameters
#'
#' @param variable name of the original variable
#' @param coefficient the coefficient used for the transformation
#' @param type type of the variable
#' @param start_variables

#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached nodes correlating to the beta_coefficient
create_beta_coefficient_nodes <-
  function(variable,
           coefficient,
           type,
           start_variables,
           PMML) {
    # Verify equal length of passed vectors
    if (!verify_length(c(variable,
                         coefficient,
                         type))) {
      stop("The length of passed params for beta_coefficient nodes does not match")
    }
    
    # Create ParameterList, FactorList, CovariateList, ParamMatrix child nodes for GeneralRegressionModel
    tmp_param_list_node <- XML::xmlNode("ParameterList")
    tmp_factor_list_node <- XML::xmlNode("FactorList")
    tmp_covariate_list_node <- XML::xmlNode("CovariateList")
    tmp_param_matrix_node <- XML::xmlNode("ParamMatrix")
    tmp_pp_matrix_node <- XML::xmlNode("PPMatrix")
    # Create a ParameterList child node Paramater with name set to p0 and label to Intercept
    tmp_param_list_node <-
      XML::addChildren(tmp_param_list_node, XML::xmlNode("Parameter", attrs = c(name = "p0", label = "Intercept")))
    # Create a ParamMatrix child node PCell with parameterName set to p0 and beta set to 0
    tmp_param_matrix_node <-
      XML::addChildren(tmp_pp_matrix_node, XML::xmlNode("PCell", attrs = c(
        parameterName = "p0", beta = "0"
      )))
    
    # Loop over the passed vectors
    for (variable_index in 1:length(variable)) {
      # Create a ParameterList child node Paramater with name set to p<loopIterator> and label set to variable
      tmp_param_list_node <-
        XML::addChildren(tmp_param_list_node, XML::xmlNode("Parameter", attrs = c(
          name = paste0("p", variable_index), label = variable[[variable_index]]
        )))
      # Check the type for cat variables create a FactorList child node Predictor with name set to variable
      # For cont variables create a CovariateList child node Predictor with name set to variable
      tmp_predictor <-
        XML::xmlNode("Predictor", attrs = variable[[variable_index]])
      switch (type[[variable_index]],
              "cont" = {
                tmp_covariate_list_node <-
                  XML::addChildren(tmp_covariate_list_node, tmp_predictor)
              },
              "cat" = {
                tmp_factor_list_node <-
                  XML::addChildren(tmp_factor_list_node, tmp_predictor)
              })
      
      # Create a PPMatrix child node PPCell with value set to 1, predictorName to variable and parameterName to p<loopIterator>
      tmp_pp_matrix_node <-
        XML::addChildren(tmp_pp_matrix_node, XML::xmlNode(
          "PPCell",
          attrs = c(
            value = "1",
            predictorName = variable[[variable_index]],
            parameterName = paste0("p", variable_index)
          )
        ))
      # Create a ParamMatrix child node PCell with parameterName set to p<loopIterator> and beta set to coefficient
      tmp_param_matrix_node <-
        XML::addChildren(tmp_param_matrix_node, XML::xmlNode(
          "PCell",
          attrs = c(
            parameterName = paste0("p", variable_index),
            beta = coefficient[[variable_index]]
          )
        ))
    }
    # Create MiningSchema child node for GeneralRegressionModel
    # Add MiningField child nodes to MiningSchema for risk and time with usageType target and active
    tmp_mining_schema_node <- XML::xmlNode("MiningSchema")
    tmp_mining_schema_node <-
      XML::addChildren(tmp_mining_schema_node,
                       XML::xmlNode("MiningField", attrs = c(name = "risk", usageType = "target")))
    tmp_mining_schema_node <-
      XML::addChildren(tmp_mining_schema_node,
                       XML::xmlNode("MiningField", attrs = c(name = "time", usageType = "active")))
    # Loop over start_variables
    for (start_variable_index in 1:length(start_variables)) {
      # Add MiningField child nodes to MiningSchema wiht name set to start_variables and usageType to active
      tmp_mining_schema_node <-
        XML::addChildren(tmp_mining_schema_node,
                         XML::xmlNode(
                           "MiningField",
                           attrs = c(name = start_variables[[start_variable_index]], usageType = "active")
                         ))
    }
    
    # Check for existence of GeneralRegressionModel node inside the PMML
    if (is.null(PMML[["GeneralRegressionModel"]])) {
      # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
      # functionName set to regression and the endTimeVariable set to time
      PMML <-
        XML::addChildren(PMML, XML::xmlNode(
          "GeneralRegressionModel",
          attrs = c(
            modelType = "coxRegression",
            functionName = "regression",
            endTimeVariable = "time"
          )
        ))
    }
    PMML[["GeneralRegressionModel"]] <-
      XML::addChildren(
        PMML[["GeneralRegressionModel"]],
        tmp_mining_schema_node,
        tmp_param_list_node,
        tmp_factor_list_node,
        tmp_covariate_list_node,
        tmp_pp_matrix_node,
        tmp_param_matrix_node
      )
    
    # Return PMML
    return(PMML)
  }

#' Create Baseline Hazards Nodes
#'
#' Creates nodes correlating to the baseline_hazards parameters
#'
#' @param time value of the corresponding time
#' @param baselineHazard value of the baseline_hazard
#' @param max_time largest time recTo value in variable_details
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached nodes correlating to the baseline_hazards
create_baseline_hazards_nodes <-
  function(time, baselineHazard, max_time, PMML) {
    # Verify equal length of passed vectors
    if (!verify_length(c(time,
                         baselineHazard))) {
      stop("The length of passed params for baseline_hazards nodes does not match")
    }
    # Create BaseCumHazardTables as child node to GeneralRegressionModel
    tmp_base_cum_hazard_table_node <-
      XML::xmlNode("BaseCumHazardTables", attrs = c(maxTime = max_time))
    # set the maxTime param to max_time
    # Loop over passed vectors
    for (vector_index in 1:length(time)) {
      # Create BaseCumHazardTables child node BaselineCell with time set to time and cumHazard to baseiline_hazard
      tmp_base_cum_hazard_table_node <-
        XML::addChildren(tmp_base_cum_hazard_table_node,
                         XML::xmlNode(
                           "BaselineCell",
                           attrs = c(time = time[[vector_index]], cumHazard = baselineHazard[[vector_index]])
                         ))
    }
    # Check for existence of GeneralRegressionModel node inside the PMML
    if (is.null(PMML[["GeneralRegressionModel"]])) {
      # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
      # functionName set to regression and the endTimeVariable set to time
      PMML <-
        XML::addChildren(PMML, XML::xmlNode(
          "GeneralRegressionModel",
          attrs = c(
            modelType = "coxRegression",
            functionName = "regression",
            endTimeVariable = "time"
          )
        ))
    }
    PMML[["GeneralRegressionModel"]] <-
      XML::addChildren(PMML[["GeneralRegressionModel"]], tmp_base_cum_hazard_table_node)
    
    # Return PMML
    return(PMML)
  }

#' Create list for each column of data
#'
#' Utility function for creating list with elements for each column of data
#'
#' @param data data.frame to be converted to list
#'
#' @return list with names set to column names from data
create_list_from_columns <- function(data) {
  ret_list <- list()
  for (column_name in colnames(data)) {
    ret_list[[column_name]] <- data[[column_name]]
  }
  return(ret_list)
}

#' Verify Lenght
#'
#' Generic function to verify identical length of all passed items
#'
#' @param items vector of objects to verify the length of
#'
#' @return boolean representing if all items share identical length
verify_length <- function(items) {
  base_length <- NULL
  for (current_item in items) {
    if (is.null(base_length)) {
      base_length <- length(current_item)
    } else if (length(current_item) == base_length) {
      next()
    } else{
      return(FALSE)
    }
  }
  return(TRUE)
}