#' subset a SingleCellExperiment to which ontology tags have been bound using 'bind_formal_tags',
#' obtaining the 'descendants' of the class of interest
#' @importFrom ontologyIndex get_descendants
#' @param sce SingleCellExperiment instance
#' @param onto representation of an ontology using representation from ontologyIndex package
#' @param class_name character(1) if 'class_tag' is missing, this will be grepped in onto[["name"]] to find class and its descendants
#' @param class_tag character(1) used if given to identify "ontological descendants" of this term in sce 
#' @return instance of SingleCellExperiment
#' @export
subset_descendants = function(sce, onto, class_name, class_tag) {
# verify 'formal' exists in sce
 stopifnot("formal" %in% names(colData(sce)))
# find the class tag if not supplied
 if (missing(class_tag)) {
   inds = grep(class_name, onto[["name"]])
   if (length(inds)==0) stop("can't find class_name in onto name component")
   if (length(inds)>1) warning("multiple matches of class_name in onto name component, using first; supply a regular expression for class_name to refine if desired")
   inds = inds[1]
   class_tag = names(onto[["name"]][inds])
 }
 desc = get_descendants(onto, class_tag)
 sce[, which(sce$formal %in% desc)]
}

