# ---------- WIP NEEDS DDI REVISIT ----------
#' #' Parses a DDI document into an R object
#' #'
#' #' Reads the DDI document on a file system and converts it into an R object.
#' #' Right now the following information is added to the object: \cr
#' #' 1. Variables info as well as values labels for categorical variables \cr
#' #' 2. Study Related Metadata
#' #'
#' #' @param ddi_path A string containing the path to the directory that has the
#' #' DDI document
#' #' @param ddi_file A string containing the name of the DDI document
#' #' @return A named list which is an instance of a BLLFlowDDI class. The list
#' #' contains the following members: \cr
#' #' 1. variable_meta_data - A named list. It's value comes from calling the \cr
#' #' \link[DDIwR]{getMetadata} function \cr
#' #' 2. additional_DDI_meta_data - A named list containig the remaining nodes in the DDI document
#' #'
#' #' @export
#' #' @examples
#' #' library(bllflow)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' read_DDI <- function(ddi_path, ddi_file) {
#'   # DDwR crates lots of cat outputs that are suppressed
#'   ddi_meta_data <-
#'     suppress_function_output(DDIwR::getMetadata(paste(ddi_path, ddi_file, sep = "/")))
#'   additional_DDI_meta_data <-
#'     xml2::as_list(xml2::read_xml(paste(ddi_path, ddi_file, sep = "/")))
#'   for (single_variable_index in 1:length(additional_DDI_meta_data$codeBook$dataDscr)) {
#'     if (!is.null(attr(additional_DDI_meta_data$codeBook$dataDscr[[single_variable_index]], "name", exact = TRUE))) {
#'       var_name <-
#'         attr(additional_DDI_meta_data$codeBook$dataDscr[[single_variable_index]],
#'              "name",
#'              exact = TRUE)
#'       names(additional_DDI_meta_data$codeBook$dataDscr)[single_variable_index] <-
#'         var_name
#'     }
#'   }
#'
#'   ddi_object <-
#'     list(variable_meta_data = ddi_meta_data, ddi_object = additional_DDI_meta_data)
#'   attr(ddi_object, "class") <-
#'     c(attr(ddi_object, "class"), "BLLFlowDDI")
#'
#'   return(ddi_object)
#' }
#'
#' # Prevents function from writing Cat to console
#' suppress_function_output <- function(x) {
#'   sink(tempfile())
#'   on.exit(sink())
#'   invisible(force(x))
#' }
#'
#' #' Parses the headers from a DDI document
#' #'
#' #' Retreives the docDscr, stdyDscr and fileDscr nodes from a DDI document, storing
#' #' them in a named list and returning the list
#' #'
#' #' @param ddi A named list created using the \code{\link{read_DDI}} function
#' #' @return Returns a named list with the following members: \cr
#' #' docDscr - Contains the docDscr node in the DDI document \cr
#' #' stdyDscr - Contains the stdyDscr node in the DDI document \cr
#' #' fileDscr - Contains the fileDscr node in the DDI document \cr
#' #'
#' #' @export
#' #' @examples
#' #' library(bllflow)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #'
#' #' pbcDDIHeaders <- bllflow::get_DDI_description(pbcDDI)
#' #' print(names(pbcDDIHeaders))
#' get_DDI_description <- function(ddi) {
#'   ddi_object <- ddi$ddi_object
#'   additional_DDI_meta_data <- list(
#'     docDscr = ddi_object$codeBook$docDscr,
#'     stdyDscr = ddi_object$codeBook$stdyDscr,
#'     fileDscr = ddi_object$codeBook$fileDscr
#'   )
#'
#'   return(additional_DDI_meta_data)
#' }
#'
#' #' Writes a variable details CSV sheet to file
#' #' @param x Object on which we will dispatch
#' #' @param ... The next method in the chain
#' #'
#' #' @export
#' write_DDI_populated_MSW <- function(x, ...) {
#'   UseMethod("write_DDI_populated_MSW", x)
#' }
#'
#' #' @describeIn write_DDI_populated_MSW The populated_variable_details data frame within a bllflow model is written
#' #' as a CSV file
#' #'
#' #' @param path_to_write_to A string containing the path to the directory
#' #' where the file should be writtem
#' #' @param new_file_name A string containing the name of the written file
#' #'
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' # Writing the variable details sheet within a bllflow model
#' #' # _________________________________________________________
#' #'
#' #' library(survival)
#' #' library(bllflow)
#' #'
#' #' data(pbc)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #'
#' #' # Read the MSW files
#' #' variables <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' #' variable_details <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #'
#' #' # Create a BLLFlow object and add labels.
#' #' pbcModel <- bllflow::BLLFlow(pbc, variables, variable_details, pbcDDI)
#' #'
#' #'
#' #' bllflow::write_DDI_populated_MSW(pbcModel, "../../inst/extdata/", "newMSWvariableDetails.csv")
#' #' }
#' #'
#' write_DDI_populated_MSW.BLLFlow <-
#'   function(x, path_to_write_to, new_file_name, ...) {
#'     bllFlow <- x
#'
#'     # create new directory if one does not exist
#'     if (!dir.exists(path_to_write_to)) {
#'       dir.create(file.path(getwd(), path_to_write_to))
#'     }
#'
#'     write.csv(bllFlow[[pkg.globals$bllFlowContent.PopulatedVariableDetails]],
#'               file = file.path(path_to_write_to, new_file_name),
#'               row.names = FALSE)
#'   }
#'
#' #' @describeIn write_DDI_populated_MSW Updates an existing variable details worksheet
#' #' with metadata from a ddi document and then writes the new variable details
#' #' sheet to file. The new sheet is saved in the same directory as the old sheet. The
#' #' first argument should be an object returned by the \code{\link{read_DDI}} function.
#' #'
#' #' @param path_to_MSW A string containing the path to the directory with the variable details sheet
#' #' @param msw_name A string containing the name of the variable details sheet
#' #' @param new_name A string containing the name of the new variable details sheet
#' #'
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' # Updating a variable details sheet from file and writing the updated version
#' #' # ___________________________________________________________________________
#' #'
#' #' library(bllflow)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #'
#' #' bllflow::write_DDI_populated_MSW(pbcDDI, "../../inst/extdata/", "PBC-variableDetails.csv", "new_name.csv")
#' #' }
#' write_DDI_populated_MSW.BLLFlowDDI <-
#'   function(x, path_to_MSW, msw_name, new_name = NULL, ...) {
#'     ddi <- x
#'
#'     if (!file.exists(file.path(path_to_MSW, msw_name))) {
#'       stop(paste("The MSW file is not present in", path_to_MSW), call. = FALSE)
#'     }
#'     variable_details <- read.csv(file.path(path_to_MSW, msw_name))
#'     populated_variable_details <-
#'       process_DDI_variable_details(ddi, variable_details)
#'
#'     # create new directory if one does not exist
#'     if (!dir.exists(path_to_MSW)) {
#'       dir.create(file.path(getwd(), path_to_MSW))
#'     }
#'
#'     # generate name for new file name if one is not provided
#'     if (is.null(new_name)) {
#'       new_name <- paste(msw_name, "DDIPopulated.csv", sep = "")
#'     }
#'
#'     write.csv(
#'       populated_variable_details,
#'       file = file.path(path_to_MSW, new_name),
#'       row.names = FALSE
#'     )
#'   }
#'
#' #' Retreives variables in a DDI document
#' #'
#' #' Returns a list of dataDscr nodes from a DDI document which represent
#' #' variables provided in the var_list argument
#' #'
#' #' @param ddi A named list returned by the \code{\link{read_DDI}} function
#' #' @param var_list A list of strings that represents variable names
#' #' @return Returns a list containing the dataDscr nodes from the DDI document,
#' #' each one of which matches with an entry in the var_list argument
#' #' @export
#' #' @examples
#' #' library(bllflow)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #'
#' #' varsForPBC <- bllflow::get_DDI_variables(pbcDDI, c("age", "sex"))
#' #' print(attr(varsForPBC[[1]], 'name'))
#' #' print(varsForPBC[[1]]$labl)
#' get_DDI_variables <- function(ddi, var_list) {
#'   ddi_variables <- list()
#'   requested_variable_indexes <-
#'     which(names(ddi$ddi_object$codeBook$dataDscr) %in% var_list)
#'   ddi_variables <-
#'     ddi$ddi_object$codeBook$dataDscr[requested_variable_indexes]
#'
#'   return(ddi_variables)
#' }
#'
#' #' Updates the model specification worksheet (MSW) of a bllflow model. Also updates
#' #' the variable details sheet with metadata from a ddi document from the original
#' #' bllflow model if it exists.
#' #'
#' #' @param bll_model A bllflow instance whose MSW will be updated
#' #' @param new_MSW_variables A dataframe containing the new MSW variables sheet
#' #' @param new_MSW_variable_deatails A dataframe containing the new MSW variable details sheet
#' #' @param new_DDI A ddi object to add to bll_model
#' #' @return  bllflow model with it's respective MSW members updated.
#' #'
#' #' @export
#' #' @examples
#' #' library(survival)
#' #' library(bllflow)
#' #'
#' #' data(pbc)
#' #'
#' #' pbcDDI <- bllflow::read_DDI(system.file("extdata", "", package="bllflow"), "pbcDDI.xml")
#' #'
#' #' # Read the MSW files
#' #' variables <- read.csv(system.file("extdata", "PBC-variables.csv", package="bllflow"))
#' #' variable_details <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package="bllflow"))
#' #'
#' #' # Create a BLLFlow object and add labels.
#' #' pbcModel <- bllflow::BLLFlow(pbc, variables, variable_details, pbcDDI)
#' #'
#' #' pbcModel <- bllflow::update_MSW(pbcModel, variables, variable_details)
#' #' pbcModel <- bllflow::update_MSW(pbcModel, variables)
#' #' pbcModel <- bllflow::update_MSW(pbcModel, new_MSW_variable_deatails = variable_details)
#' update_MSW <- function(bll_model,
#'                       new_MSW_variables = NULL,
#'                       new_MSW_variable_deatails = NULL,
#'                       new_DDI = NULL) {
#'   if (!is.null(new_DDI)) {
#'     bll_model[[pkg.globals$bllFlowContent.DDI]] <- new_DDI
#'     bll_model[[pkg.globals$bllFlowContent.PopulatedVariableDetails]] <-
#'       process_DDI_variable_details(bll_model[[pkg.globals$bllFlowContent.DDI]], bll_model[[pkg.globals$bllFlowContent.VariableDetails]])
#'     bll_model[[pkg.globals$bllFlowContent.AdditionalMetaData]] <- get_DDI_description(new_DDI)
#'   }
#'   if (!is.null(new_MSW_variables)) {
#'     bll_model[[pkg.globals$bllFlowContent.Variables]] <- new_MSW_variables
#'   }
#'   if (!is.null(new_MSW_variable_deatails)) {
#'     bll_model[[pkg.globals$bllFlowContent.VariableDetails]] <-
#'       new_MSW_variable_deatails
#'     if (!is.null(bll_model[[pkg.globals$bllFlowContent.DDI]])) {
#'       bll_model[[pkg.globals$bllFlowContent.PopulatedVariableDetails]] <-
#'         process_DDI_variable_details(bll_model[[pkg.globals$bllFlowContent.DDI]], new_MSW_variable_deatails)
#'     }
#'   }
#'
#'   return(bll_model)
#' }
