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
model_csv_to_pmml <- function(model_export, export_path = FALSE) {
  # Create a list of read in csv files needed to create the PMML
  # list format of:model_specification_list = list(name_of_file = list(fileType = fileType, table = readFile))
  model_specification_list <-
    chain_read_files(model_export, "initial_spec")
  
  # Create PMML
  working_PMML <- PMML_template_creation_code_chunc()
  
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
#' 
#' @return pmml nodes
convert_file_to_PMML <- function(file_list_element){
  working_file <- file_list_element[[file]]
  
  # Loop over every row of the file converting it to a node then adding them together
  working_PMML <- codeChunkToCreateTheBase
  for (row_index in nrow(working_file)) {
    
  }
  
  return(working_PMML)
}