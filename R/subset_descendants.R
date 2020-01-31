#' subset a SummarizedExperiment to which ontology tags have been bound using 'bind_formal_tags',
#' obtaining the 'descendants' of the class of interest
#' @importFrom ontologyIndex get_descendants
#' @param se SummarizedExperiment instance
#' @param onto representation of an ontology using representation from ontologyIndex package
#' @param class_name character(1) if 'class_tag' is missing, this will be grepped in onto[["name"]] to find class and its descendants
#' @param class_tag character(1) used if given to identify "ontological descendants" of this term in se 
#' @param formal_cd_name character(1) tells name used for ontology tag column in `colData(se)`
#' @return instance of SummarizedExperiment
#' @export
subset_descendants = function(se, onto, class_name, class_tag, formal_cd_name='label.ont') {
# verify [formal_cd_name] exists in se
 stopifnot(formal_cd_name %in% names(colData(se)))
# find the class tag if not supplied
 if (missing(class_tag)) {
   inds = grep(class_name, onto[["name"]])
   if (length(inds)==0) stop("can't find class_name in onto name component")
   if (length(inds)>1) warning("multiple matches of class_name in onto name component, using first; supply a regular expression for class_name to refine if desired")
   inds = inds[1]
   class_tag = names(onto[["name"]][inds])
 }
 desc = get_descendants(onto, class_tag)
 se[, which(se[[formal_cd_name]] %in% desc)]
}

