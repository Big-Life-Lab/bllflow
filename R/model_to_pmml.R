#' Model csv to PMML
#'
#' Using the passed model_export csv, files needed to build the model are read in.
#' Then, the model instructions are converted to PMML and if export_path is passed
#' it is then saved to that path.
#'
#' @param model_export csv file containing the files needed for the model
#' @param export_path path and file name to export the PMML to
#'
#' @return PMML of the model specified in the model_export csv
#'
model_csv_to_pmml <-
  function(model_export, data_name, export_path = FALSE) {
    # Read in variables sheet and variable details pass the rest of the rows to chain read
    # Create a list of read in csv files needed to create the PMML
    # list format of:model_specification_list = list(name_of_file = list(fileType = fileType, table = readFile))
    model_specification_list <-
      chain_read_files(model_export, "initial_spec")
    
    # Create PMML
    working_PMML <- PMML_template_creation_code_chunc()
    
    # use variables sheet to obtain list of variables used
    # Using the variables list trim variable details to only contain rows relavant to variables
    # Loop over the new variable details one variable at a time working with all rows that contain that one variable
    # Build the DataField and DerivedField node using the variable details rows for a single variable
    # Loop over the rest of the rows creating the full DataDictionary and TransformationDictionary for this database
    
    # Pass the remaining model_specs and PMML to convert_file_to_PMML
    
    
    
    while (length(model_specification_list) > 0) {
      PMML_nodes <- convert_file_to_PMML(model_specification_list[[1]])
      
      # Not sure if insert_nodes will be a function or a code chunc but the code would reflect the name
      working_PMML <-
        insert_nodes(working_PMML, PMML_nodes, model_specification_list[[1]][["file_name"]])
      
      model_specification_list <- model_specification_list[[-1]]
    }
    
    return(working_PMML)
    
  }

#' Chain Read Files
#'
#' Recursive function tasked with reading in n number of files needed for the model
#'
#' @param file_path|file path or the file itself
#' @param file_type type of file being read. Needed for list creation
#'
#' @return list object containing the name of the file, the file_type and the file itself
#'
chain_read_files <- function(file_path, file_type) {
  # Scoping declaration
  working_file <- NULL
  file_list <- list()
  
  # Checking for file rather then path
  if (is.character(file_path)) {
    # Read in the file
    working_file <- read.csv2(file_path)
  } else{
    working_file <- file_path
  }
  
  # Check if file contains other files to read
  if ("filePath" %in% colnames(working_file)) {
    # Loop over every row and call chain_read on those files
    for (row_index in nrow(working_file)) {
      working_row <- working_file[row_index,]
      
      new_file_path <- working_row[["filePath"]]
      new_file_type <- working_row[["fileType"]]
      # Not sure if will be needed also will not be function
      file_name <- extractNameFromFilePath(new_file_path)
      tmp_list_element <-
        chain_read_files(new_file_path, new_file_type)
      tmp_list_element[["file_name"]] <- file_name
      
      file_list <- append(file_list, tmp_list_element)
    }
  } else{
    file_list <-
      append(
        file_list,
        list(
          file = working_file,
          file_path = file_path,
          file_type = file_type,
          file_name = file_name
        )
      )
  }
  
  # Structure the list for proper output
  if (file_type == "initial_spec") {
    
  }
  
  
  return(file_list)
}

#' Convert file to PMML
#'
#' Converts passed file list element into PMML nodes
#'
#' @param file_list_element an element from the file_list
#' @param PMML the pmml to append to
#'
#' @return pmml nodes
convert_file_to_PMML <- function(file_list_element, PMML) {
  
}

#' Create generic node
#'
#' Responsible for creating generic nodes
#'
#' @param node_name the name of the node being made
#' @param node_attributes a named list containing node attributes
#' @param end_contains_name a boolean FALSE = end type of "/>", TRUE = "</node_name>"
#' @param additional_body optional param containing content to insert between opening and closing tags
#'
#' @return a string containing the assembled node
create_generic_node <-
  function(node_name,
           node_attributes,
           end_contains_name,
           additional_body = "") {
    node_start <- paste0("<", node_name)
    node_end <- "/>"
    if (end_contains_name) {
      node_start <- paste0(node_start, "/>")
      node_end <- paste0("</", node_name, ">")
    }
    node_body <- ""
    for (attribute_name in names(node_attributes)) {
      node_body <-
        paste0(node_body,
               " ",
               attribute_name,
               "=\"",
               node_attributes[[attribute_name]],
               "\"")
    }
    
    return(paste0(node_start, node_body, additional_body, node_end))
  }

#' Create Dummy Nodes
#'
#' Creates dummy nodes given passed parameters. The passed params must all be of same length
#'
#' @param orig_variable a vector representing the original variable name
#' @param cat_value a vector representing the original variable cat value
#' @param dummy_variable a vector representing the new dummy variable name
#' @param PMML the pmml that is appended to
#'
#' @return PMML containing the newly added DerivedFields
create_dummy_nodes <-
  function(orig_variable,
           cat_value,
           dummy_variable,
           PMML) {
    # Verify matching length
    if (!(
      length(orig_variable) == length(cat_value) &&
      length(orig_variable) == length(dummy_variable)
    )) {
      stop(paste0(
        "The length of orig_variable, cat_valuem, dummy_variable are not all equal"
      ))
    }
    new_nodes <- "\n"
    for (row_index in length(orig_variable)) {
      const_vector <- c()
      const_vector[[cat_value[[row_index]]]] <-
        create_generic_node("Constant", list(dataType = "float"), TRUE, cat_value[[row_index]])
      for (float_value in 0:1) {
        const_vector[[float_value]] <-
          create_generic_node("Constant", list(dataType = "float"), TRUE, float_value)
      }
      field_ref <-
        create_generic_node("FieldRef", list(field = orig_variable[[row_index]]), FALSE)
      apply_sub_node <-
        create_generic_node("Apply",
                            list(func = "if"),
                            TRUE,
                            paste(field_ref, const_vector[[cat_value[[row_index]]]], sep = "\n"))
      tmp_node <-
        create_generic_node(
          "DerivedField",
          list(
            name = dummy_variable[[row_index]],
            optype = "categorical",
            dataType = "float"
          ),
          TRUE,
          paste(apply_sub_node, const_vector[[1]], const_vector[[0]], sep = "\n")
        )
      tmp_node <- stringr::str_replace(tmp_node, "func", "function")
      new_nodes <- paste(new_nodes, tmp_node, sep = "\n")
    }
    
    # Attach the newly made nodes to the TransformationDictionary of the passed PMML
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
    # Verify matching length  @RUSTY Consider exporting to function
    if (!(
      length(orig_variable) == length(center_value) &&
      length(orig_variable) == length(centered_variable) &&
      length(orig_variable) == length(centered_variable_type)
    )) {
      stop(paste0(
        "The length of orig_variable, cat_valuem, dummy_variable are not all equal"
      ))
    }
    
    new_nodes <- "\n"
    for (row_index in length(orig_variable)) {
      field_ref <-
        create_generic_node("FieldRef", list(field = orig_variable[[row_index]]), FALSE)
      const <-
        create_generic_node("Constant", list(dataType = "float"), TRUE, center_value[[row_index]])
      apply_sub_node <-
        create_generic_node("Apply",
                            list(func = "-"),
                            TRUE,
                            paste(field_ref, const, sep = "\n"))
      
      if (centered_variable_type[[row_index]] == "cat") {
        centered_variable_type[[row_index]] <- "categorical"
      } else if (centered_variable_type[[row_index]] == "cat") {
        centered_variable_type[[row_index]] <- "continuous"
      }
      
      tmp_node <-
        create_generic_node(
          "DerivedField",
          list(
            name = centered_variable[[row_index]],
            optype = centered_variable_type[[row_index]],
            dataType = "float"
          ),
          TRUE,
          apply_sub_node
        )
      
      tmp_node <- stringr::str_replace(tmp_node, "func", "function")
      new_nodes <- paste(new_nodes, tmp_node, sep = "\n")
    }
    # Attach the newly made nodes to the TransformationDictionary of the passed PMML
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
  if (!(length(variable) == length(rcs_variables) &&
        length(variable) == length(knots))) {
    stop(paste0(
      "The length of orig_variable, cat_valuem, dummy_variable are not all equal"
    ))
  }
  new_nodes <- "\n"
  for (row_index in length(variable)) {
    rcs_constant_array_node <-
      create_generic_node("Array", list(n = "5", type = "float"), FALSE, knots[[row_index]])
    
    first_rcs_node_field_ref <-
      create_generic_node("FieldRef", list(field = variable[[row_index]]), FALSE)
    first_rcs_node_apply <-
      create_generic_node("Apply",
                          list(func = "equal"),
                          TRUE,
                          first_rcs_node_field_ref)
    first_rcs_node <-
      create_generic_node(
        "DerivedField",
        list(name = rcs_variables[[row_index]][[1]], optype = "continuous"),
        TRUE,
        first_rcs_node_apply
      )
    
    new_nodes <- paste(new_nodes, first_rcs_node, sep = "\n")
    for (rcs_previous_variable_index in 1:(length(rcs_variables[[row_index]]) -
                                           1)) {
      rcs_variable_index <- rcs_previous_variable_index + 1
      rcs_variable_node_field_ref <-
        create_generic_node("FieldRef", list(field = rcs_variables[[row_index]][[rcs_previous_variable_index]]), FALSE)
      rcs_variable_node_constant <-
        create_generic_node("Constant",
                            list(dataType = "float"),
                            TRUE,
                            rcs_variable_index)
      
      rcs_variable_node_apply <-
        create_generic_node(
          "Apply",
          list(func = "rcs"),
          TRUE,
          paste(
            rcs_variable_node_field_ref,
            rcs_variable_node_constant,
            rcs_constant_array_node,
            sep = "\n"
          )
        )
      
      rcs_variable_node <-
        create_generic_node(
          "DerivedField",
          list(name = rcs_variables[[row_index]][[rcs_variable_index]], optype = "continuous"),
          TRUE,
          rcs_variable_node_apply
        )
      new_nodes <- paste(new_nodes, rcs_variable_node, sep = "\n")
    }
  }
  # Append the new nodes to TransformationDictionary
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
    if (!(
      length(interaction_variable) == length(interaction_variable_type) &&
      length(interaction_variable) == length(interacting_variables)
    )) {
      stop(paste0(
        "The length of orig_variable, cat_valuem, dummy_variable are not all equal"
      ))
    }
    new_nodes <- "\n"
    for (row_index in length(interaction_variable)) {
      interacting_node_list <- list()
      for (single_interacting_variable in interacting_variables[[row_index]]) {
        interacting_node_list[[single_interacting_variable]] <-
          create_generic_node("FieldRef",
                              list(field = single_interacting_variable),
                              FALSE)
      }
      interaction_node_apply <-
        create_generic_node(
          "Apply",
          list(func = "*"),
          TRUE,
          paste(interacting_node_list[[1]], interacting_node_list[[2]], sep = "\n")
        )
      
      interaction_node <-
        create_generic_node(
          "DerivedField",
          list(
            name = interaction_variable[[row_index]],
            optype = interaction_variable_type[[row_index]],
            dataType = "float"
          ),
          TRUE,
          interaction_node_apply
        )
      
      new_nodes <- paste(new_nodes, interaction_node, sep = "\n")
    }
    
    # Append the new nodes to TransformationDictionary
  }

#' Create Beta Coefficient Nodes
#' 
#' Creates nodes correlating to the beta_coefficient parameters
#' 
#' @param variable name of the original variable
#' @param coefficient the coefficient used for the transformation
#' @param type type of the variable
#' @param PMML the pmml that is appended to
#' 
#' @return returns PMML with attached nodes correlating to the beta_coefficient
create_beta_coefficient_nodes <-
  function(variable, coefficient, type, PMML) {
    # Add the time DataField node to the DataDictionary
    # Read in every column to their respective vectors
    # Create a MiningSchema and add MiningField nodes for risk and time
    # Append additional MiningField nodes for each variable
    # Create ParameterList with a starting node of p0 with label Intercept
    # Append additional Parameter nodes following the p<n> structure for the remaining variables
    # Create FactorList using any variables that have a cat value in the type column as Predictor nodes
    # Repeat above step to create CovariateList for any variables of cont type
    # Create the PPMatrix with a PPCell node for each variable and a matching parameterName to that in ParameterList
    # Create ParamMatrix with initial PCell node of p0 and beta 0
    # Create additional nodes for the remaining variables following the p<n> pattern and matching them to the coefficient column
    # Check for existence of GeneralRegressionModel in the PMML if present append the newly created nodes inside it if its not present
    # Create new GeneralRegressionModel using the specified template
  }

#' Create Baseline Hazards Nodes
#' 
#' Creates nodes correlating to the baseline_hazards parameters
#' 
#' @param time value of the corresponding time
#' @param baseline_hazard value of the baseline_hazard
#' @param PMML the pmml that is appended to
#' 
#' @return returns PMML with attached nodes correlating to the baseline_hazards
create_baseline_hazards_nodes <- function(time, baseline_hazard, PMML){
# Create a BaseCumHazardTables node with maxTime set to nrow
# Create a BaselineCell node for each row using the time and baselineHazard columns
# Check for existence of GeneralRegressionModel in the PMML if present append the newly created nodes inside it if its not present
# Create new GeneralRegressionModel using the specified template
}
