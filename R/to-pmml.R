#' Creates a PMML document from a broom-processed object.
#'
#' @param obj Data frame to be converted to PMML.
#'
#' @return A PMML document.
#'
#' @examples
#'
#' @export
to_pmml <- function(obj) {
  doc <- XML::xmlNode(pkg.env$node_name.pmml, namespaceDefinitions=c(pkg.env$node_namespace.pmml), attrs=c(version=pkg.env$node_attr.pmml_version))
  dict <- XML::xmlNode(pkg.env$node_name.data_dict)
  trans_dict <- XML::xmlNode(pkg.env$node_name.trans_dict)
  model <- XML::xmlNode(pkg.env$node_name.model)
  return (XML::append.xmlNode(doc, dict, trans_dict, model))
}
