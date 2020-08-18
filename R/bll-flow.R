# DDI support has been dropped for now
# if (!is.null(ddi)) {
#   # TODO redisign to create template rather then populate add a check to verify proper structure
#   # processedVariableDetails <-
#   #   ProcessDDIVariableDetails(ddi, variable_details)
#
#   check_for_existance_of_in_list(c("variableMetaData", "ddiObject"),
#                                  ddi,
#                                  paste(names(ddi), "ddi"))
#   ddi_header[[names(ddi)]] <-
#     get_DDI_description(ddi)
#
#
# } else{
#   ddi_header <- NULL
# }