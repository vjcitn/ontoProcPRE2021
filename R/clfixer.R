#' obtain named character vector of terms from Cell Line Ontology, omitting obsolete and trailing 'cell'
#' @return character()
#' @examples
#' cleanCLOnames()[1:10]
#' @export
cleanCLOnames = function() {
 requireNamespace("ontoProc")
 offic = getCellLineOnto()$name # has ID bound as names()
 isobs = grep("obso", offic)
 if (length(isobs)>0) offic = offic[-isobs]
 offic = sub(" cell$", "", offic)
}


chker = function(x,y,n=1,...) {d = adist(x,y,...); y[order(d,na.last=TRUE)][seq_len(n)]}

#' repair nomenclature mismatches (to curated term set) in a vector of terms
#' @importFrom utils adist
#' @param cand character vector of candidate terms
#' @param namedOffic named character vector of curated terms, the names are regarded as tags, intended to be identifiers in curated ontologies
#' @param n numeric(1) number of nearest neighbors to return
#' @param tagcolname character(1) prefix used to name columns for tags in output
#' @param \dots passed to \code{\link[utils]{adist}}
#' @return a data.frame instance with 2n+1 columns (column 1 is candidate,
#' remaining n pairs of columns are (term, tag) for n nearest neighbors
#' as measured by \code{adist}.
#' @examples
#' candidates = c("JHH7", "HUT102", "HS739T", "NCIH716")
#' # the candidates are cell line names returned in the text dump from
#' # https://portals.broadinstitute.org/ccle/page?gene=AHR
#' # note that one must travel to the third nearest neighbor
#' # to find the match (and tag) for Hs 739.T
#' # in this example, we compare to cell line names in Cell Line Ontology
#' nomenCheckup(candidates, cleanCLOnames(), n=3, tagcolname="clo")
#' @export
nomenCheckup = function(cand, namedOffic, n=1, tagcolname="tag", ...) {
 ans = lapply(cand, function(x) chker(x, namedOffic, n=n, ...))
 fin = vector("list", 1L+2*n)
 fin[[1]] = cand
 loc = 1 
 nms = vector("character", 2*n)
 for (j in seq(2, 2*n, 2)) {
   fin[[j]] = vapply(ans, function(x)as.character(x)[loc], character(1))
   fin[[j+1]] = vapply(ans, function(x)names(x)[loc], character(1))
   nms[j-1] = paste0("hit", j/2)
   nms[j] = paste0(tagcolname, j/2)
   loc = loc+1
   }   
 names(fin) = c("cand", nms)
 data.frame(fin)
}

