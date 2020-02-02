#' add mapping from informal to formal cell type tags to a SummarizedExperiment colData
#' @param se SummarizedExperiment instance
#' @param informal character(1) name of colData element with uncontrolled vocabulary
#' @param tagmap data.frame with columns 'informal' and 'formal'
#' @param force logical(1), defaults to FALSE; if TRUE, allows clobbering existing colData variable named "formal"
#' @return SummarizedExperiment instance with a new colData column 'label.ont' giving
#' the formal tags associated with each sample
#' @note This function will fail if the value of `informal` is not among the
#' colData variable names, or if "formal" is among the colData variable names.
#' @export
bind_formal_tags = function(se, informal, tagmap, force=FALSE) {
  cdn = names(colData(se))
  if (!(informal %in% cdn)) stop(sprintf("can't find %s in names(colData(se))", informal))
  if ("formal" %in% cdn && !force) stop("won't clobber existing colData variable 'formal', please rename or set force = TRUE")
  if (!(all(c("informal", "formal") %in% names(tagmap)))) stop("tagmap must include columns named 'formal' and 'informal'")
  mapv = tagmap[["formal"]]
  names(mapv) = tagmap[["informal"]]
  colData(se)$label.ont = mapv[se[[informal]]]
  se
}
