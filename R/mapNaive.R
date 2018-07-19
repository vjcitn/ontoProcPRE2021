#' use grep or agrep to find a match for a naive token into ontology
#' @param naive character(1)
#' @param onto an instance of ontologyIndex::ontology_index
#' @param useAgrep logical(1) if TRUE, agrep will be used
#' @param \dots passed to agrep if used
#' @return if a match is found, the result of grep/agrep with value=TRUE is returned; otherwise a named NA_character_ is returned
#' @examples
#' co = ontoProc::getCellOnto()
#' mapOneNaive("astrocyte", co)
#' @return named vector, names are ontology identifiers, values are matched strings
#' @export
mapOneNaive = function(naive, onto, useAgrep=FALSE, ...) {
  stopifnot(is.atomic(naive) , is.character(naive))
  stopifnot(inherits(onto, "ontology_index"))
  if (useAgrep) return(agrep(naive, onto$name, value=TRUE, ...))
  ans = grep(naive, onto$name, value=TRUE)
  if (length(ans)==0) { 
    ans = NA_character_
    names(ans) = naive
    return(ans)
    }
   ans
}

#' select a set of elements from a term 'map' and return a contribution to a data.frame
#' @param namedvec named character vector, as returned from \code{\link{mapOneNaive}}
#' @param index numeric() or integer(), typically of length one
#' @return a data.frame; if \code{index} does not inherit from
#' \code{numeric}, a data.frame of one row with columns 'ontoid'
#' and 'term' populated with \code{NA_character_} is returned,
#' otherwise a similarly named data.frame is returned with
#' contents from the selected elements of \code{namedvec}
#' @examples
#' co = ontoProc::getCellOnto()
#' mast = mapOneNaive("astrocyte", co)
#' selectFromMap(mast, 1)
#' @export 
selectFromMap = function(namedvec, index) {
  if (!(inherits(index, "numeric")|inherits(index, "integer"))) return(
    data.frame(ontoid=NA_character_, term=NA_character_)
    )
  ans = namedvec[index]
  data.frame(ontoid=names(ans), term=as.character(ans))
}

#' Produce a data.frame with a set of naive terms mapped to all matching ontology ids and their formal terms
#' @param terms character() vector, can use grep-compatible regular expressions
#' @param onto an instance of ontologyIndex::ontology_index
#' @param useAgrep logical(1) if TRUE, agrep will be used
#' @param \dots passed to agrep if used
#' @return a data.frame
#' @examples
#' cands = c("astrocyte$", "oligodendrocyte", "oligodendrocyte precursor",
#'    "neoplastic", "^neuron$", "^vascular", "badterm")
#' co = ontoProc::getCellOnto()
#' liberalMap(cands, co)
#' @export
liberalMap = function(terms, onto, useAgrep=FALSE, ...) {
 maps = lapply(terms, function(x) mapOneNaive(x, onto, useAgrep, ...))
 lens = sapply(maps, length)
 inputs = rep(terms,lens)
 ans = do.call(rbind, lapply(seq_len(length(maps)), function(x)
    selectFromMap(maps[[x]], seq_len(lens[x]))))
 ans = cbind(input=inputs, ans)
 dups = which(duplicated(ans[, "ontoid"]))
 if (length(dups)>0) return(ans[-dups,])
 ans
}
