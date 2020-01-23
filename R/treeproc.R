isLeaf = function(x, ont) {
 ch = children(x, ont);
 if (is.na(ch[1])) return(TRUE)
 if (length(ch)<1) return(TRUE) 
 return(FALSE)
}

children = function(x, ont) ont$children[[x]]

.getLeavesFromTerm = function(x, ont) {
  if (isLeaf(x, ont)) return(x)
  lapply(children(x, ont),
     function(z) .getLeavesFromTerm(z, ont))
}

#' obtain childless descendents of a term (including query)
#' @param x a character(1) id element for ontology_index instance
#' @param ont an ontology_index instance as defined in ontologyIndex package
#' @return character vector of 'leaves' of ontology tree
#' @examples
#' ch = getChebiOnto()
#' alldr = getLeavesFromTerm("CHEBI:23888", ch)
#' head(ch$name[alldr[1:15]])
#' @export
getLeavesFromTerm = function(x, ont) {
 unlist(.getLeavesFromTerm(x, ont))
} 

