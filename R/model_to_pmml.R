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
model_csv_to_pmml <- function(model_export, data_name, export_path = FALSE) {
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
  
  # Pass the remaining model_specs alongside variables list and PMML to convert_file_to_PMML
  
  
  
  while(length(model_specification_list)>0){
    PMML_nodes <- convert_file_to_PMML(model_specification_list[[1]])
    
    # Not sure if insert_nodes will be a function or a code chunc but the code would reflect the name
    working_PMML <- insert_nodes(working_PMML, PMML_nodes, model_specification_list[[1]][["file_name"]])
    
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
    file_list <- append(file_list, list(file = working_file, file_path = file_path, file_type = file_type, file_name = file_name))
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
convert_file_to_PMML <- function(file_list_element, PMML){
  # For dummy step files
  # Loop over each row adding a seperate DerivedField node per row
  # Attach the newly made nodes to the TransformationDictionary of the passed PMML
  
  # For center steps
  # Loop over each row adding new DerivedFields based on the centeredVariable column
  # Attach the newly made nodes to the TransformationDictionary of the passed PMML
  
  # For rcs steps
  # Loop over each row of the file
  # For each element in the rcsVariables column create a new DerivedFields node
  # The first node is set to equal
  # Loop over the remaing values creating seperate nodes
  # Append the new nodes to TransformationDictionary
  
  # For interaction steps
  # Loop over the rows of the file
  # For each row create a new node with a name from the interactionVariable 
  # Append the nodes
  
  # For beta-coefficients fileType steps
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
  
  # For baseline-hazards fileType steps
  # Create a BaseCumHazardTables node with maxTime set to nrow
  # Create a BaselineCell node for each row using the time and baselineHazard columns
  # Check for existence of GeneralRegressionModel in the PMML if present append the newly created nodes inside it if its not present
  # Create new GeneralRegressionModel using the specified template
  
  
  
}