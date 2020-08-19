#' list and count samples with common ontological annotation in two SEs
#' @param ont instance of ontologyIndex ontology
#' @param se1 a SummarizedExperiment using `label.ont` in colData
#' to provide ontological tags (from `ont`) for samples
#' @param se2 a SummarizedExperiment using `label.ont` in colData
#' to provide ontological tags (from `ont`) for samples
#' @return a data.frame with rownames given by the
#' common tags, the class names as column `clname`, and
#' counts of samples bearing the given tags in remaining columns.
#' @examples
#' if (requireNamespace("celldex")) {
#'   imm = celldex::ImmGenData()
#'   if ("label.ont" %in% names(colData(imm))) {
#'     cl = getCellOnto()
#'     blu = celldex::BlueprintEncodeData()
#'     common_classes( cl, imm, blu )
#'     }
#'   }
#' @export
common_classes = function(ont, se1, se2) {
 stopifnot("label.ont" %in% names(colData(se1)))
 stopifnot("label.ont" %in% names(colData(se2)))
 comm = intersect(se1$label.ont, se2$label.ont)
 stopifnot(length(comm)>1)
 t1 = table(se1$label.ont[se1$label.ont %in% comm])
 if (any(is.na(t1))) message("an NA detected in se1")
 t2 = table(se2$label.ont[se2$label.ont %in% comm])
 if (any(is.na(t2))) message("an NA detected in se2")
 n1 = deparse(substitute(se1))
 n2 = deparse(substitute(se2))
 ans = data.frame(clname=ont$name[comm], t1=as.numeric(t1[comm]), t2=as.numeric(t2[comm]))
 rownames(ans) = comm
 names(ans)[2:3] = c(n1, n2)
 ans
}

