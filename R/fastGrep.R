#' some fields of interest are lists, and grep per se should not be used -- this function checks and uses grep within vapply when appropriate
#' @param patt a regular expression whose presence in field should be checked
#' @param onto an ontologyIndex instance
#' @param field the ontologyIndex component to be searched
#' @param \dots passed to grep
#' @return logical vector indicating vector or list elements where a match is found
#' @examples
#' cheb = getChebiOnto()
#' ind = fastGrep("17-AAG", cheb, "synonym")
#' cheb$name[ind]
#' @export
fastGrep = function(patt, onto, field, ...) {
 stopifnot(field %in% names(onto))
 resource = onto[[field]]
 if (is.list(resource))
   return(which(vapply(onto[[field]], function(x) length(grep(patt, x, ...))>0, logical(1))))
 else if (is.character(resource)) return(grep(patt, resource, ...))
 else stop("onto[[field]] neither a list nor character vector")
}
