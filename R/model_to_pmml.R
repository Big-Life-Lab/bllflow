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
convert_model_export_to_pmml <- function(model_export_file_path, database_name) {
  # Read in variables and variable-details saving them to appropriate variables
  # Read in model-steps and creating a list for each row of model-steps
  # Loop over above list and read in each file specified saving it in a list containing the step name and fileType as well as the content of the file
  
  # Create empty PMML doc using variables from strings.R to fill in name and version of the package
  
  # Using recodeflow::recode_to_pmml append the empty PMML with DataDictionary and TransformationDictionary
  # Calculate max_time from variable_details recTo column
  
  # Loop over the list elements from model-steps
  # Convert the read in file into seperate vectors for each column
  # For columns containing multiple elements in 1 cell check for existence of ;
  # Convert the above detected columns into nested vectors 
  # Use the fileType attribute to decide which function is called to process the newly created vectors
  # Add the produced PMML to the current PMML and move on to the next iteration of the loop
  
  # Return the PMML
  
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
    # Loop over the length of the vectors
    # For each iteration of the loop create a DerivedField node 
    # The name attribute is set using dummy_variable value
    # the optype attribute is set to categorical
    # The dataType attribute is set to integer
    # Create an Apply node with function attribute set to if
    # To the Apply node add children nodes of FieldRef and Constant
    # The FieldRef field attibute is populated using orig_variable
    # The Constant dataType attibute is set to float and the value is populated from cat_value
    # Add the Apply node and two constant Constant nodes as children to the DerivedField node
    # The 2 constant Constant nodes have dataType of float and values of 0 and 1 
    # Add the DerivedField to the return PMML
    # At the end of the loop return the PMML
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
    # Loop over the lenght of the vectors
    # For each iteration create a DerivedField node 
    # The name attribute comes from centered_variable
    # the optype is determined by centered_variable_type cont = continues, cat = categorical
    # The dataType is determined by centered_variable_type cont = float, cat = string
    # Create a child Apply node with function attribute set to -
    # Create a child FieldRef node for Apply node with the field attribute populated by orig_variable
    # Create a child Constant node for Apply node with dataType attribute set to float and its value set by center_value
    # Add the DerivedField node to the TransformationDictionary of the passed PMML
    
    # Return the PMML at the end of the loop
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
  # Loop over the length of the passed vectors
  # Create the first rcs DerivedField node
  # Create a temporary Constant Array Node with n set to 5, type set to float, and values from knots
  # The name attribute is set to the first element of the current index of rcs_variables
  # optype attribute is set to continuous and dataType attribute is set to float
  # Create a child Apply node set its attribute function to equal
  # Add child FieldRef node to Apply with attribute field set to variable
  # Add the DerivedField as a child node to TransformationDictionary
  # Create nested loop of rcs_variables current index nested list starting at +1 from start of length
  # Create DerivedField node with attribute name set to rcs_loop_index inside the nested rcs_variables list
  # optype is set to continues and dataType is set to float
  # Add child Apply node with function node of rcs
  # To the Apply node add fieldRef child node with field set to rcs_loop_index-1 inside the nested rcs_variables list
  # To the Apply node add Constant child node with datatType set to float and value of rcs_loop_index
  # To the Apply node add the temporary constant Array node created in the parent loop
  # Add the DerivedField as a child node to TransformationDictionary
  
  # Return the PMML
 
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
    # Loop over passed vectors
    # Create DerivedField node with name attribute set by interaction_variable column
    # optype attribute is determined based on interaction_variable_type cont= continues, cat = categorical
    # dataType attribute is determined based on interaction_variable_type cont= float, cat = string
    # Create Apply child node with function attribute set to *
    # Add length(interacting_variables nested lins) FieldRef child nodes to Apply node with field attribute set by the nested interacting_variables list 
    # Add the DerivedField as a child node to the PMML TransformationDictionary
    
    # Return PMML
    
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
    # Verify equal length of passed vectors
    # Check for existence of GeneralRegressionModel node inside the PMML
    # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
    # functionName set to regression and the endTimeVariable set to time
    # Create MiningSchema, ParameterList, FactorList, CovariateList, ParamMatrix child nodes for GeneralRegressionModel
    # Create a MiningSchema child node MiningField with name set to risk and usageType set to target
    # Repeat above step with name set to time and usageType to active
    # Create a ParameterList child node Paramater with name set to p0 and label to Intercept
    # Create a ParamMatrix child node PCell with parameterName set to p0 and beta set to 0
    # Loop over the passed vectors
    # Create a MiningSchema child node MiningField with name set to variable and usageType to active
    # Create a ParameterList child node Paramater with name set to p<loopIterator> and label set to variable
    # Check the type for cat variables create a FactorList child node Predictor with name set to variable
    # For cont variables create a CovariateList child node Predictor with name set to variable
    # Create a PPMatrix child node PPCell with value set to 1, predictorName to variable and parameterName to p<loopIterator>
    # Create a ParamMatrix child node PCell with parameterName set to p<loopIterator> and beta set to coefficient
    
    # Return PMML
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
create_baseline_hazards_nodes <- function(time, baseline_hazard, max_time, PMML){
  # Verify equal length of passed vectors
  # Check for existence of GeneralRegressionModel node inside the PMML
  # If no node is found create GeneralRegressionModel node with modelType set to CoxRegression
  # functionName set to regression and the endTimeVariable set to time
  # Create BaseCumHazardTables as child node to GeneralRegressionModel
  # set the maxTime param to max_time
  # Loop over passed vectors
  # Create BaseCumHazardTables child node BaselineCell with time set to time and cumHazard to baseiline_hazard
  
  # Return PMML
  
}
