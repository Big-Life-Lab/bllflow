#' Converts the CSv files that are part of a model export into PMML
#'
#' @param model_export_file_path string The absolute or relative path to the
#' model export file. If relative, the path should relative to the working
#' directory of the project
#' @param database_name string The database from which to take the starting variables
#' for the model
#' @param custom_function_files string vector An optional list of paths to R files 
#' that have functions referenced by the variable details sheet. Each of these
#' will be converted to PMML and added
#' @return An object created by the XML::xmlNode function that represents the
#' PMML XML
#'
#' @export
#'
#' @examples
convert_model_export_to_pmml <-
  function(model_parameters_folder_path, model_export_file_path, database_name, custom_function_files = NULL) {
    model_export <-
      read.csv(model_export_file_path,
               fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE)
    
    variables_path <-
      model_export[model_export$fileType == pkg.globals$ModelExportCSV.variables, pkg.globals$ModelStepsCSV.filePath]
    variables_path <-
      file.path(dirname(model_export_file_path), variables_path)
    
    variable_details_path <-
      model_export[model_export$fileType == pkg.globals$ModelExportCSV.variable_details, pkg.globals$ModelStepsCSV.filePath]
    variable_details_path <-
      file.path(dirname(model_export_file_path), variable_details_path)
    
    model_steps_path <-
      model_export[model_export$fileType == pkg.globals$ModelExportCSV.model_steps, pkg.globals$ModelStepsCSV.filePath]
    model_steps_path <-
      file.path(dirname(model_export_file_path), model_steps_path)
    
    # Read in variables and variable-details saving them to appropriate variables
    variables <-
      read.csv(variables_path,
               fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE)
    variable_details <-
      read.csv(variable_details_path,
               fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE)
    
    # Read in model-steps and creating a list for each row of model-steps
    model_steps <-
      read.csv(model_steps_path,
               fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE)
    detected_steps <-
      unique(model_steps[[pkg.globals$ModelStepsCSV.step]])
    step_list <- list()
    
    for (working_step in detected_steps) {
      current_rows <-
        model_steps[model_steps[[pkg.globals$ModelStepsCSV.step]] == working_step, c(pkg.globals$ModelStepsCSV.fileType,
                                                                                     pkg.globals$ModelStepsCSV.filePath)]
      step_list[[working_step]] <-
        list(fileType = current_rows[[pkg.globals$ModelStepsCSV.fileType]], filePath = current_rows[[pkg.globals$ModelStepsCSV.filePath]])
    }
    
    # Loop over above list and read in each file specified saving it in a list containing the step name and fileType as well as the content of the file
    if (length(step_list) >= 1) {
      for (single_step in 1:length(step_list)) {
        # Check for sub steps
        if (length(step_list[[single_step]][[1]]) > 1) {
          for (sub_step in 1:length(step_list[[single_step]][[1]])) {
            step_list[[single_step]][[pkg.globals$ModelInternal.file]][[sub_step]] <-
              read_step_file(model_export_file_path, step_list[[single_step]][[pkg.globals$ModelStepsCSV.filePath]][[sub_step]])
          }
        } else{
          step_list[[single_step]][[pkg.globals$ModelInternal.file]] <-
            read_step_file(model_export_file_path, step_list[[single_step]][[pkg.globals$ModelStepsCSV.filePath]])
        }
      }
    }
    # Create empty PMML doc using variables from strings.R to fill in name and version of the package
    doc <-
      XML::xmlNode(
        pkg.globals$PMML.Node.PMML,
        namespaceDefinitions = c(
          pkg.globals$PMML.Node.Attributes.Value.xmlns,
          xsi = pkg.globals$PMML.Node.Attributes.Value.xsi
        ),
        attrs = c(version = pkg.globals$PMML.Node.Attributes.Value.PMML.Version)
      )
    header <- XML::xmlNode(pkg.globals$PMML.Node.Header)
    header <-
      XML::addChildren(header,
                       XML::xmlNode(
                         pkg.globals$PMML.Node.Application,
                         attrs = c(name = "bllflow", version = as.character(packageVersion("bllflow")))
                       ))
    
    
    # Using recodeflow::recode_to_pmml append the empty PMML with DataDictionary and TransformationDictionary
    recodeflow_pmml <-
      recodeflow::recode_to_pmml(
        var_details_sheet = variable_details,
        vars_sheet = variables,
        db_name = database_name,
        custom_function_files = custom_function_files
      )
    doc <-
      XML::addChildren(doc, header, recodeflow_pmml[[pkg.globals$PMML.Node.DataDictionary]], recodeflow_pmml[[pkg.globals$PMML.Node.TransformationDictionary]])
    working_pmml <- doc
    
    # Calculate max_time from variable_details recTo column
    max_time <- 0
    # Vector containing all matching variable start variables based on database name
    all_start_vars <- c()
    if (pkg.globals$variables.Time %in% variable_details[[pkg.globals$argument.Variables]]) {
      variable_details_time_rows <- variable_details[variable_details[[pkg.globals$argument.Variables]] == pkg.globals$variables.Time, ]
      
      max_time <-
        max(as.character(variable_details_time_rows[[pkg.globals$argument.recEnd]]))
      min_time <-
        min(as.character(variable_details_time_rows[[pkg.globals$argument.recEnd]]))
      working_pmml[[pkg.globals$PMML.Node.DataDictionary]] <-
        XML::addChildren(
          working_pmml[[pkg.globals$PMML.Node.DataDictionary]],
          XML::xmlNode(
            pkg.globals$PMML.Node.DataField,
            attrs = c(
              name = pkg.globals$variables.Time,
              displayName = "time of predicted probability",
              optype = pkg.globals$PMML.Node.Attributes.Value.optype.cont,
              dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float
            ),
            XML::xmlNode(
              pkg.globals$PMML.Node.Extension,
              attrs = c(
                name = pkg.globals$PMML.Extension.names.units,
                value = variable_details_time_rows[1, ][[pkg.globals$argument.Units]]
              )
            ),
            XML::xmlNode(
              pkg.globals$PMML.Node.Extension,
              attrs = c(
                name = pkg.globals$PMML.Extension.names.variableStartLabel,
                value = ""
              )
            ),
            XML::xmlNode(
              pkg.globals$PMML.Node.Interval,
              attrs = c(
                closure = "closedClosed",
                leftMargin = min_time,
                rightMargin = max_time,
                property = "valid"
              )
            )
          )
        )
      # Create a vector containing all variables from variableStart matching database name
      all_unique_vars <-
        unique(variable_details[grepl(database_name, variable_details[[pkg.globals$argument.DatabaseStart]]), pkg.globals$argument.Variables])
      for (end_variable in all_unique_vars) {
        working_row <-
          variable_details[variable_details[[pkg.globals$argument.Variables]] == end_variable,]
        working_row <- working_row[1, ]
        
        # If this row is for a derived variable then don't get the start variable
        # for it. This is because the start variables for derived variables
        # are variables from the Variable column.
        if(!recodeflow:::is_derived_var(working_row)) {
          # Check whether the start variable for this variable is a leaf
          # in the dependency tree. If it isn't then don't add it to the 
          # list of variables which should be MiningFields
          start_var_name <- recodeflow:::get_start_var_name(working_row, database_name)
          if(start_var_name %in% variable_details[[pkg.globals$argument.Variables]] == FALSE) {
            all_start_vars <-
              append(
                all_start_vars,
                start_var_name
              )
          }
        }
      }
    }
    
    # Loop over the list elements from model-steps
    for (step_name in names(step_list)) {
      working_step <- step_list[[step_name]]
      
      if (length(working_step[[pkg.globals$ModelStepsCSV.fileType]]) > 1) {
        for (sub_step_index in 1:length(working_step[[pkg.globals$ModelStepsCSV.fileType]])) {
          working_pmml <-
            convert_step(
              current_file_type = working_step[[pkg.globals$ModelStepsCSV.fileType]][[sub_step_index]],
              step_name,
              current_file = working_step[[pkg.globals$ModelInternal.file]][[sub_step_index]],
              all_start_vars,
              max_time,
              working_pmml,
              model_parameters_folder_path
            )
        }
      } else{
        working_pmml <-
          convert_step(
            current_file_type = working_step[[pkg.globals$ModelStepsCSV.fileType]],
            step_name,
            current_file = working_step[[pkg.globals$ModelInternal.file]],
            all_start_vars,
            max_time,
            working_pmml,
            model_parameters_folder_path
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
#' @param orig_variable a vector representing the original variable name
#' @param cat_value a vector representing the original variable cat value
#' @param dummy_variable a vector representing the new dummy variable name
#' @param PMML to append to
#'
#' @return PMML containing the newly added DerivedFields
create_dummy_nodes <-
  function(orig_variable,
           cat_value,
           dummy_variable,
           PMML) {
    # Verify matching length of all passed vectors
    if (!verify_length(c(orig_variable, cat_value, dummy_variable))) {
      stop("The length of passed params for dummy nodes does not match")
    }
    
    # Loop over the length of the vectors
    for (vector_index in 1:length(orig_variable)) {
      # For each iteration of the loop create a DerivedField node
      # The name attribute is set using dummy_variable value
      # the optype attribute is set to categorical
      # The dataType attribute is set to integer
      current_main_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.DerivedField,
          attrs = c(
            name = dummy_variable[[vector_index]],
            optype = pkg.globals$PMML.Node.Attributes.Value.optype.cat,
            dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float
          )
        )
      # Create an Apply node with function attribute set to if
      current_apply_node_parent <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Apply,
          attrs = c('function' = pkg.globals$PMML.Node.Attributes.Value.function.if)
        )
      current_apply_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Apply,
          attrs = c('function' = pkg.globals$PMML.Node.Attributes.Value.function.equal)
        )
      # To the Apply node add children nodes of FieldRef and Constant
      # The FieldRef field attibute is populated using orig_variable
      # The Constant dataType attibute is set to float and the value is populated from cat_value
      current_field_node <-
        XML::xmlNode(pkg.globals$PMML.Node.FieldRef,
                     attrs = c(field = orig_variable[[vector_index]]))
      current_const_apply_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Constant,
          attrs = c(dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float),
          cat_value[[vector_index]]
        )
      current_apply_node <-
        XML::addChildren(current_apply_node,
                         current_field_node,
                         current_const_apply_node)
      
      current_apply_node_parent <-
        XML::addChildren(current_apply_node_parent, current_apply_node)
      
      current_const_0 <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Constant,
          attrs = c(dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float),
          0
        )
      current_const_1 <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Constant,
          attrs = c(dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float),
          1
        )
      current_apply_node_parent <- XML::addChildren(
        current_apply_node_parent,
        current_const_1,
        current_const_0
      )
      # The 2 constant Constant nodes have dataType of float and values of 0 and 1
      current_main_node <-
        XML::addChildren(
          current_main_node,
          current_apply_node_parent
        )
      # Add the DerivedField to the return PMML
      PMML[[pkg.globals$PMML.Node.TransformationDictionary]] <-
        XML::addChildren(PMML[[pkg.globals$PMML.Node.TransformationDictionary]], current_main_node)
    }
    return(PMML)
  }

#' Create Center Nodes
#'
#' Creates center nodes according to passed params
#'
#' @param orig_variable the original variable name
#' @param center_value value to center on
#' @param centered_variable name of the centered variable
#' @param centered_variable_type type of the new centered variable
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached centered nodes
create_center_nodes <-
  function(orig_variable,
           center_value,
           centered_variable,
           centered_variable_type,
           PMML) {
    # Verify matching length of vectors
    if (!verify_length(c(
      orig_variable,
      center_value,
      centered_variable,
      centered_variable_type
    ))) {
      stop("The length of passed params for center nodes does not match")
    }
    # Loop over the lenght of the vectors
    for (vector_index in 1:length(orig_variable)) {
      # For each iteration create a DerivedField node
      # The name attribute comes from centered_variable
      # the optype is determined by centered_variable_type cont = continuous, cat = categorical
      # The dataType is determined by centered_variable_type cont = float, cat = string
      optype <- ""
      data_type <- ""
      cat_cont_conversion <-
        cont_and_cat_converter(centered_variable_type[[vector_index]])
      optype <- cat_cont_conversion$optype
      data_type <- cat_cont_conversion$data_type
      
      current_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.DerivedField,
          attrs = c(
            name = centered_variable[[vector_index]],
            optype = optype,
            dataType = data_type
          )
        )
      # Create a child Apply node with function attribute set to -
      # Create a child FieldRef node for Apply node with the field attribute populated by orig_variable
      # Create a child Constant node for Apply node with dataType attribute set to float and its value set by center_value
      current_apply_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Apply,
          attrs = c('function' = pkg.globals$PMML.Node.Attributes.Value.function.minus)
        )
      current_field_node <-
        XML::xmlNode(pkg.globals$PMML.Node.FieldRef,
                     attrs = c(field = orig_variable[[vector_index]]))
      current_constant_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Constant,
          attrs = c(dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float),
          center_value[[vector_index]]
        )
      current_apply_node <-
        XML::addChildren(current_apply_node,
                         current_field_node,
                         current_constant_node)
      # Add the DerivedField node to the TransformationDictionary of the passed PMML
      current_node <-
        XML::addChildren(current_node, current_apply_node)
      PMML[[pkg.globals$PMML.Node.TransformationDictionary]] <-
        XML::addChildren(PMML[[pkg.globals$PMML.Node.TransformationDictionary]], current_node)
    }
    # Return the PMML at the end of the loop
    return(PMML)
  }

#' Create RCS nodes
#'
#' Creates RCS nodes in the TransformationDictionary
#'
#' @param variable list of original variables
#' @param rcs_variables list containing nested lists of rcs_variables for each original variable
#' @param knots list containing the knots
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached rcs nodes
create_rcs_nodes <- function(variable, rcs_variables, knots, PMML) {
  # Verify matching length
  if (!verify_length(c(variable,
                       rcs_variables,
                       knots))) {
    stop("The length of passed params for rcs nodes does not match")
  }
  # Loop over the length of the passed vectors
  for (vector_index in 1:length(variable)) {
    variable_list <-
      strsplit(rcs_variables[[vector_index]], pkg.globals$variables.splitValue)[[1]]
    # Create the first rcs DerivedField node
    # Create a temporary Constant Array Node with n set to 5, type set to float, and values from knots
    current_knots_raw <-
      strsplit(knots[[vector_index]], pkg.globals$variables.splitValue)[[1]]
    current_knots <- paste(current_knots_raw, collapse = " ")
    current_array_node <-
      XML::xmlNode(
        pkg.globals$PMML.Node.Array,
        attrs = c(
          n = length(current_knots_raw),
          type = pkg.globals$PMML.Node.Attributes.Value.Type.float
        ),
        current_knots
      )
    # The name attribute is set to the first element of the current index of rcs_variables
    # optype attribute is set to continuous and dataType attribute is set to float
    # Create a child Apply node set its attribute function to equal
    # Add child FieldRef node to Apply with attribute field set to variable
    # Add the DerivedField as a child node to TransformationDictionary
    current_first_node <-
      XML::xmlNode(
        pkg.globals$PMML.Node.DerivedField,
        attrs = c(
          name = variable_list[[1]],
          optype = pkg.globals$PMML.Node.Attributes.Value.optype.cont,
          dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float
        )
      )
    current_first_field_node <-
      XML::xmlNode(pkg.globals$PMML.Node.FieldRef, attrs = c(field = variable[[vector_index]]))
    current_first_node <-
      XML::addChildren(
        current_first_node,
        current_first_field_node
      )
    PMML[[pkg.globals$PMML.Node.TransformationDictionary]] <-
      XML::addChildren(PMML[[pkg.globals$PMML.Node.TransformationDictionary]], current_first_node)
    
    # Create nested loop of rcs_variables current index nested list starting at +1 from start of length
    for (rcs_variable_index in 2:length(variable_list)) {
      # Create DerivedField node with attribute name set to rcs_loop_index inside the nested rcs_variables list
      # optype is set to continuous and dataType is set to float
      current_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.DerivedField,
          attrs = c(
            name = variable_list[[rcs_variable_index]],
            optype = pkg.globals$PMML.Node.Attributes.Value.optype.cont,
            dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float
          )
        )

      # Add child Apply node with function node of rcs
      # To the Apply node add fieldRef child node with field set to the first rcs
      # variable name for the current row
      # To the Apply node add Constant child node with datatType set to float and value of rcs_loop_index
      current_apply_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Apply,
          attrs = c('function' = pkg.globals$PMML.Node.Attributes.Value.function.rcs)
        )
      current_field_node <-
        XML::xmlNode(pkg.globals$PMML.Node.FieldRef,
                     attrs = c(field = variable_list[[1]]))
      current_const <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Constant,
          attrs = c(dataType = pkg.globals$PMML.Node.Attributes.Value.dataType.float),
          rcs_variable_index
        )
      # To the Apply node add the temporary constant Array node created in the parent loop
      current_apply_node <-
        XML::addChildren(current_apply_node,
                         current_field_node,
                         current_const,
                         current_array_node)
      current_node <-
        XML::addChildren(current_node, current_apply_node)
      # Add the DerivedField as a child node to TransformationDictionary
      PMML[[pkg.globals$PMML.Node.TransformationDictionary]] <-
        XML::addChildren(PMML[[pkg.globals$PMML.Node.TransformationDictionary]], current_node)
    }
  }
  # Return the PMML
  return(PMML)
}

#' Create Interaction Nodes
#'
#' Creates interaction nodes in the TransformationDictionary
#'
#' @param interaction_variable list of interaction_variable names
#' @param interaction_variable_type list of interaction_variable types
#' @param interacting_variables list of interacting variable names
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached interaction nodes
create_interaction_nodes <-
  function(interaction_variable,
           interaction_variable_type,
           interacting_variables,
           PMML) {
    # Verify matching length
    if (!verify_length(c(
      interaction_variable,
      interaction_variable_type,
      interacting_variables
    ))) {
      stop("The length of passed params for interaction nodes does not match")
    }
    # Loop over passed vectors
    for (vector_index in 1:length(interaction_variable)) {
      variable_list <-
        strsplit(interacting_variables[[vector_index]],
                 pkg.globals$variables.splitValue)[[1]]
      # Create DerivedField node with name attribute set by interaction_variable column
      # optype attribute is determined based on interaction_variable_type cont= continuous, cat = categorical
      # dataType attribute is determined based on interaction_variable_type cont= float, cat = string
      optype <- ""
      data_type <- ""
      cat_cont_conversion <-
        cont_and_cat_converter(interaction_variable_type[[vector_index]])
      optype <- cat_cont_conversion$optype
      data_type <- cat_cont_conversion$data_type
      
      current_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.DerivedField,
          attrs = c(
            name = interaction_variable[[vector_index]],
            optype = optype,
            dataType = data_type
          )
        )
      # Create Apply child node with function attribute set to *
      current_apply_node <-
        XML::xmlNode(
          pkg.globals$PMML.Node.Apply,
          attrs = c(
            'function' = pkg.globals$PMML.Node.Attributes.Value.function.multiplication
          )
        )
      
      # Add length(interacting_variables nested lins) FieldRef child nodes to Apply node with field attribute set by the nested interacting_variables list
      for (interacting_variable in variable_list) {
        current_apply_node <-
          XML::addChildren(current_apply_node,
                           XML::xmlNode(
                             pkg.globals$PMML.Node.FieldRef,
                             attrs = c(field = interacting_variable)
                           ))
      }
      
      current_node <-
        XML::addChildren(current_node, current_apply_node)
      # Add the DerivedField as a child node to the PMML TransformationDictionary
      PMML[[pkg.globals$PMML.Node.TransformationDictionary]] <-
        XML::addChildren(PMML[[pkg.globals$PMML.Node.TransformationDictionary]], current_node)
      
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
#' @param mining_fields

#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached nodes correlating to the beta_coefficient
create_beta_coefficient_nodes <-
  function(variable,
           coefficient,
           type,
           mining_fields,
           PMML) {
    # Verify equal length of passed vectors
    if (!verify_length(c(variable,
                         coefficient,
                         type))) {
      stop("The length of passed params for beta_coefficient nodes does not match")
    }
    
    # Create ParameterList, FactorList, CovariateList, ParamMatrix child nodes for GeneralRegressionModel
    current_param_list_node <-
      XML::xmlNode(pkg.globals$PMML.Node.ParameterList)
    # Create a ParameterList child node Paramater with name set to p0 and label to Intercept
    current_param_list_node <-
      XML::addChildren(
        current_param_list_node,
        XML::xmlNode(
          pkg.globals$PMML.Node.Parameter,
          attrs = c(
            name = pkg.globals$PMML.Node.Attributes.Value.startVar,
            label = pkg.globals$PMML.Node.Attributes.Value.label.Intercept
          )
        )
      )
    current_param_matrix_node <-
      XML::xmlNode(pkg.globals$PMML.Node.ParamMatrix)
    # Create a ParamMatrix child node PCell with parameterName set to p0 and beta set to 0
    current_param_matrix_node <-
      XML::addChildren(current_param_matrix_node,
                       XML::xmlNode(
                         pkg.globals$PMML.Node.PCell,
                         attrs = c(
                           parameterName = pkg.globals$PMML.Node.Attributes.Value.startVar,
                           beta = "0"
                         )
                       ))
    current_factor_list_node <-
      XML::xmlNode(pkg.globals$PMML.Node.FactorList)
    current_covariate_list_node <-
      XML::xmlNode(pkg.globals$PMML.Node.CovariateList)
    current_pp_matrix_node <-
      XML::xmlNode(pkg.globals$PMML.Node.PPMatrix)
    
    
    # Loop over the passed vectors
    for (variable_index in 1:length(variable)) {
      # Create a ParameterList child node Paramater with name set to p<loopIterator> and label set to variable
      current_param_list_node <-
        XML::addChildren(
          current_param_list_node,
          XML::xmlNode(
            pkg.globals$PMML.Node.Parameter,
            attrs = c(
              name = paste0(
                pkg.globals$PMML.Node.Attributes.Value.Var.incrementVar,
                variable_index
              ),
              label = variable[[variable_index]]
            )
          )
        )
      # Check the type for cat variables create a FactorList child node Predictor with name set to variable
      # For cont variables create a CovariateList child node Predictor with name set to variable
      current_predictor <-
        XML::xmlNode(pkg.globals$PMML.Node.Predictor, attrs = c(name = variable[[variable_index]]))
      # cont and cat cannot be assigned to variables as switch uses them as case names
      # Bypass is a #TODO
      switch (
        type[[variable_index]],
        "cont" = {
          current_covariate_list_node <-
            XML::addChildren(current_covariate_list_node, current_predictor)
        },
        "cat" = {
          current_factor_list_node <-
            XML::addChildren(current_factor_list_node, current_predictor)
        }
      )
      
      # Create a PPMatrix child node PPCell with value set to 1, predictorName to variable and parameterName to p<loopIterator>
      current_pp_matrix_node <-
        XML::addChildren(current_pp_matrix_node,
                         XML::xmlNode(
                           pkg.globals$PMML.Node.PPCell,
                           attrs = c(
                             value = "1",
                             predictorName = variable[[variable_index]],
                             parameterName = paste0(
                               pkg.globals$PMML.Node.Attributes.Value.Var.incrementVar,
                               variable_index
                             )
                           )
                         ))
      # Create a ParamMatrix child node PCell with parameterName set to p<loopIterator> and beta set to coefficient
      current_param_matrix_node <-
        XML::addChildren(
          current_param_matrix_node,
          XML::xmlNode(
            pkg.globals$PMML.Node.PCell,
            attrs = c(
              parameterName = paste0(
                pkg.globals$PMML.Node.Attributes.Value.Var.incrementVar,
                variable_index
              ),
              beta = coefficient[[variable_index]]
            )
          )
        )
    }
    # Create MiningSchema child node for GeneralRegressionModel
    # Add MiningField child nodes to MiningSchema for risk and time with usageType target and active
    current_mining_schema_node <-
      XML::xmlNode(pkg.globals$PMML.Node.MiningSchema)
    current_mining_schema_node <-
      XML::addChildren(
        current_mining_schema_node,
        XML::xmlNode(
          pkg.globals$PMML.Node.MiningField,
          attrs = c(
            name = "risk",
            usageType = pkg.globals$PMML.Node.Attributes.Value.usageType.target
          )
        )
      )
    current_mining_schema_node <-
      XML::addChildren(
        current_mining_schema_node,
        XML::xmlNode(
          pkg.globals$PMML.Node.MiningField,
          attrs = c(
            name = pkg.globals$variables.Time,
            usageType = pkg.globals$PMML.Node.Attributes.Value.usageType.active
          )
        )
      )
    # Loop over mining_fields
    for (start_variable_index in 1:length(mining_fields)) {
      # Add MiningField child nodes to MiningSchema wiht name set to mining_fields and usageType to active
      current_mining_schema_node <-
        XML::addChildren(
          current_mining_schema_node,
          XML::xmlNode(
            pkg.globals$PMML.Node.MiningField,
            attrs = c(
              name = mining_fields[[start_variable_index]],
              usageType = pkg.globals$PMML.Node.Attributes.Value.usageType.active
            )
          )
        )
    }
    
    # Check for existence of GeneralRegressionModel node inside the PMML
    if (is.null(PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]])) {
      # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
      # functionName set to regression and the endTimeVariable set to time
      PMML <-
        XML::addChildren(PMML,
                         XML::xmlNode(
                           pkg.globals$PMML.Node.GeneralRegressionModel,
                           attrs = c(
                             modelType = pkg.globals$PMML.Node.Attributes.Value.modelType.CoxRegression,
                             functionName = pkg.globals$PMML.Node.Attributes.Value.modelFunction.regression,
                             endTimeVariable = pkg.globals$variables.Time
                           )
                         ))
    }
    PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]] <-
      XML::addChildren(
        PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]],
        current_mining_schema_node,
        current_param_list_node,
        current_factor_list_node,
        current_covariate_list_node,
        current_pp_matrix_node,
        current_param_matrix_node
      )
    
    # Return PMML
    return(PMML)
  }

#' Create Baseline Hazards Nodes
#'
#' Creates nodes correlating to the baseline_hazards parameters
#'
#' @param time value of the corresponding time
#' @param baseline_hazard value of the baseline_hazard
#' @param max_time largest time recTo value in variable_details
#' @param PMML the pmml that is appended to
#'
#' @return returns PMML with attached nodes correlating to the baseline_hazards
create_baseline_hazards_nodes <-
  function(time, baseline_hazard, max_time, PMML) {
    # Verify equal length of passed vectors
    if (!verify_length(c(time,
                         baseline_hazard))) {
      stop("The length of passed params for baseline_hazards nodes does not match")
    }
    # Create BaseCumHazardTables as child node to GeneralRegressionModel
    current_base_cum_hazard_table_node <-
      XML::xmlNode(pkg.globals$PMML.Node.BaseCumHazardTables,
                   attrs = c(maxTime = max_time))
    # set the maxTime param to max_time
    # Loop over passed vectors
    for (vector_index in 1:length(time)) {
      # Create BaseCumHazardTables child node BaselineCell with time set to time and cumHazard to baseiline_hazard
      current_base_cum_hazard_table_node <-
        XML::addChildren(
          current_base_cum_hazard_table_node,
          XML::xmlNode(
            pkg.globals$PMML.Node.BaselineCell,
            attrs = c(time = time[[vector_index]], cumHazard = baseline_hazard[[vector_index]])
          )
        )
    }
    # Check for existence of GeneralRegressionModel node inside the PMML
    if (is.null(PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]])) {
      # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
      # functionName set to regression and the endTimeVariable set to time
      PMML <-
        XML::addChildren(PMML,
                         XML::xmlNode(
                           pkg.globals$PMML.Node.GeneralRegressionModel,
                           attrs = c(
                             modelType = pkg.globals$PMML.Node.Attributes.Value.modelType.CoxRegression,
                             functionName = pkg.globals$PMML.Node.Attributes.Value.modelFunction.regression,
                             endTimeVariable = pkg.globals$variables.Time
                           )
                         ))
    }
    PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]] <-
      XML::addChildren(PMML[[pkg.globals$PMML.Node.GeneralRegressionModel]], current_base_cum_hazard_table_node)
    
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

node_creation_switch <-
  function(current_file_type,
           current_file,
           working_pmml,
           all_start_vars,
           max_time,
           model_parameters_folder_path) {
    working_pmml <-  switch(
      current_file_type,
      'dummy' = {
        create_dummy_nodes(
          orig_variable = current_file$origVariable,
          cat_value = current_file$catValue,
          dummy_variable = current_file$dummyVariable,
          PMML = working_pmml
        )
      },
      'center' = {
        # Make the environment in which we will evaluate any expression strings
        # in the centering file. The environment will consist of all the model
        # parameters files in the folder as data frames. The name of each file 
        # in the folder will be its name in the environment
        model_parameter_file_paths <- list.files(
          model_parameters_folder_path,
          pattern = ".csv",
          full.names = TRUE
        )

        parsed_model_parameter_files <- list()
        for(model_parameter_file_path in model_parameter_file_paths) {
          model_parameter_file_name <- gsub(
            ".csv",
            "",
            basename(model_parameter_file_path)
          )
          parsed_model_parameter_files[[model_parameter_file_name]] <- read.csv(
            model_parameter_file_path,
            fileEncoding = "UTF-8-BOM"
          )
        }
        
        # Replace any expression strings in the centerValue column with its
        # evaluated value
        new_center_value <- current_file$centerValue
        # The regex to check whether the center value is a regex string
        expression_regex <- "^(.{1,})\\[(.{1,}), {0,}\\] {0,}\\$(.{1,})$"
        for(center_value_index in seq_len(length(current_file$centerValue))) {
          # If it is a regex string
          if(grepl(expression_regex, current_file$centerValue[center_value_index])) {
            expression_value <- eval(
              str2lang(current_file$centerValue[center_value_index]),
              envir = parsed_model_parameter_files
            )
            suppressWarnings(
              coerced_expression_value <- as.numeric(expression_value)
            )
            # Check whether there was a row found in a model parameter file for
            # the expression
            if(identical(coerced_expression_value, numeric(0))) {
              stop(paste("Error interpolating ", current_file$centerValue[center_value_index], ". No row found in reference file for expression.", sep = ""))  
            }
            if(is.na(coerced_expression_value)) {
              stop(paste("Error interpolating ", current_file$centerValue[center_value_index], ". Value ", expression_value, " should be a number but is ", typeof(expression_value), " and could not be coerced to a number", sep = ""))
            }
            new_center_value[center_value_index] <- coerced_expression_value
          }
        }
        
        create_center_nodes(
          orig_variable = current_file$origVariable,
          center_value = new_center_value,
          centered_variable = current_file$centeredVariable,
          centered_variable_type = current_file$centeredVariableType,
          PMML = working_pmml
        )
      },
      'rcs' = {
        create_rcs_nodes(
          variable = current_file$variable,
          rcs_variables = current_file$rcsVariables,
          knots = current_file$knots,
          PMML = working_pmml
        )
      },
      'interaction' = {
        create_interaction_nodes(
          interaction_variable = current_file$interactionVariable,
          interaction_variable_type = current_file$interactionVariableType,
          interacting_variables = current_file$interactingVariables,
          PMML = working_pmml
        )
      },
      'beta-coefficients' = {
        create_beta_coefficient_nodes(
          variable = current_file$variable,
          coefficient = current_file$coefficient,
          type = current_file$type,
          mining_fields = all_start_vars,
          PMML = working_pmml
        )
      },
      'baseline-hazards' = {
        create_baseline_hazards_nodes(
          time = current_file$time,
          baseline_hazard = current_file$baselineHazard,
          max_time = max_time,
          PMML = working_pmml
        )
      }
    )
    
    return(working_pmml)
  }

cont_and_cat_converter <- function(switch_value) {
  optype <- ""
  data_type <- ""
  # cont and cat cannot be assigned to variables as switch uses them as case names
  # Bypass is a #TODO
  switch (
    switch_value,
    "cont" = {
      optype <- pkg.globals$PMML.Node.Attributes.Value.optype.cont
      data_type <-
        pkg.globals$PMML.Node.Attributes.Value.dataType.float
    },
    "cat" = {
      optype = pkg.globals$PMML.Node.Attributes.Value.optype.cat
      data_type <-
        pkg.globals$PMML.Node.Attributes.Value.dataType.string
    }
  )
  
  return(list(optype = optype, data_type = data_type))
}

read_step_file <- function(model_export_file_path, step_path) {
  current_file_path <-
    file.path(dirname(model_export_file_path), step_path)
  current_file <-
    read.csv(current_file_path,
             fileEncoding = "UTF-8-BOM",
             stringsAsFactors = FALSE)
  
  return(current_file)
}

convert_step <-
  function(current_file_type,
           step_name,
           current_file,
           all_start_vars,
           max_time,
           working_pmml,
           model_parameters_folder_path) {
    if (is.na(current_file_type) | current_file_type == "") {
      current_file_type <- step_name
    }
    
    working_pmml <-
      node_creation_switch(
        current_file_type = current_file_type,
        current_file = current_file,
        working_pmml = working_pmml,
        all_start_vars = all_start_vars,
        max_time = max_time,
        model_parameters_folder_path = model_parameters_folder_path
      )
    
    return(working_pmml)
  }

