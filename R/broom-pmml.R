#' Creates a PMML document from a broom-processed object.
#'
#' @param input Data frame to be converted to PMML.
#'
#' @return A PMML document.
#'
#' @examples
#'
#' @export
broom.pmml <- function(input) {
  doc <- XML::xmlNode("PMML", namespaceDefinitions=c("http://www.dmg.org/PMML-4_4"), attrs=c(version="4.4"))
  return (doc)
}
