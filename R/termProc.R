
.tsvalidity = function(object) {
   chk1 = (length(object@ontoTags)==nrow(object@cleanFrame))
   chk2 = all(names(object@cleanFrame) %in% c("clean", "tag", "parent"))
   report = ""
   if (!chk1) report = "ontoTags length != nrow(cleanFrame)"
   if (!chk2) report = paste0(report, "...cleanFrame column names not as expected")
   if (report == "") return(TRUE)
   return(report)
}

# Nota bene: this class was defined to handle formal URIs in OWL
# we are backing down to simple tags now that we are using
# ontologyIndex package with OBO imports

#' manage ontological data with tags and a DataFrame instance
#' @rdname TermSet-class
#' @import ontologyIndex
#' @importFrom Biobase selectSome
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors DataFrame
#' @importFrom methods show new as
#' @return instance of TermSet
#' @exportClass TermSet
setClass("TermSet", representation(ontoTags="character", 
   cleanFrame="DataFrame"), validity=.tsvalidity)
#' abbreviated display for TermSet instances
#' @rdname TermSet-class
#' @aliases show
#' @aliases show,TermSet-method
#' @param object instance of TermSet class
#' @examples
#' efoOnto = getEFOOnto()
#' defsibs = siblings_TAG("EFO:1001209", efoOnto)
#' class(defsibs)
#' defsibs
#' @exportMethod show
#' @export
setMethod("show", "TermSet", function(object) {
cat(sprintf("TermSet for %d terms\n", length(object@ontoTags)))
cat(paste(selectSome(object@cleanFrame[,"clean"]), collapse=", "), "\n")
})
#' combine TermSet instances
#' @param x TermSet instance
#' @param \dots additional instances
#' @return TermSet instance
#' @export
setMethod("c", "TermSet", function(x, ...) {
  if (missing(x)) args = unname(list(...))
  else args = unname(list(x, ...))
  if (length(args)==1) return(args)
  j <- seq_len(length(args))
  ansu <- unlist(lapply(args, function(elt) elt@ontoTags))
  ansf <- do.call(rbind, lapply(args, function(elt) elt@cleanFrame))
  new("TermSet", ontoTags = ansu, cleanFrame=ansf)
})

#' generate a TermSet with siblings of a given term, excluding that term by default
#' @param Tagstring a character(1) that identifies a term
#' @param ontology instance of ontology_index (S3) from ontologyIndex
#' @param justSibs character(1)
#' @return TermSet instance
#' @examples
#' efoOnto = getEFOOnto()
#' siblings_TAG( "EFO:1001209", efoOnto )
#' @export
siblings_TAG = function(Tagstring="EFO:1001209", ontology, justSibs=TRUE ) {
   stopifnot(length(Tagstring)==1)
   stopifnot(Tagstring %in% ontology$id)
   parents = ontology$parents[which(ontology$id == Tagstring)][[1]]
   sibs = setdiff(unlist(ontology$children[ unlist(parents) ]), Tagstring)
   if (!justSibs) sibs = c(sibs, Tagstring)
   clnTerms = ontology$name[sibs]
   Tags = sibs
   parent = rep(paste(parents, collapse=":"), length(clnTerms))
   o = order(clnTerms)
   new("TermSet", ontoTags=Tags[o], cleanFrame=
          DataFrame(clean=clnTerms[o], tag=Tags[o], parent=parent[o]))
}

#' acquire the label of an ontology subject tag
#' @rdname siblings_TAG
#' @aliases label_TAG
#' @return character(1)
#' @note for \code{label_TAG}, \code{Tagstring} may be a vector
#' @examples
#' efoOnto = getEFOOnto()
#' label_TAG( "EFO:0000311", efoOnto )
#' @export
label_TAG = function(Tagstring="EFO:0000311", ontology) {
  ontology$name[ Tagstring ]
}

#' acquire the labels of children of an ontology subject tag
#' @rdname siblings_TAG
#' @aliases children_TAG
#' @examples
#' efoOnto = getEFOOnto()
#' children_TAG( ontology = efoOnto )
#' @return TermSet instance
#' @export
children_TAG = function(Tagstring="EFO:1001209", ontology ) {
   stopifnot(all(Tagstring %in% ontology$id))
   chil = unlist(ontology$children[ Tagstring ])
   clnTerms = ontology$name[chil]
   Tags = chil
   parent = rep(paste(Tagstring, collapse=":"), length(clnTerms))
   o = order(clnTerms)
   new("TermSet", ontoTags=Tags[o], cleanFrame=
          DataFrame(clean=clnTerms[o], tag=Tags[o], parent=parent[o]))
}

#' utilities for approximate matching of cell type terms to GO categories and annotations
#' @importFrom stats na.omit
#' @importFrom AnnotationDbi select
#' @param celltypeString character atom to be used to search GO terms using 
#' @param orgDb instances of orgDb
#' @param cols columns to be retrieved in select operation
#' @param gotab a data.frame with columns GO (goids) and TERM (term strings)
#' \code{\link[base]{agrep}}
#' @param \dots additional arguments to \code{\link[base]{agrep}}
#' @note Very primitive, uses agrep to try to find relevant terms.
#' @return data.frame
#' @examples
#' library(org.Hs.eg.db)
#' head(cellTypeToGO("serotonergic neuron", ontoProc::allGOterms))
#' head(cellTypeToGenes("serotonergic neuron", ontoProc::allGOterms, org.Hs.eg.db))
#' @export
cellTypeToGO = function(celltypeString, gotab,...) {
 gotab[agrep(celltypeString, gotab[,2],...),]
 }
#' @rdname cellTypeToGO
#' @return data.frame
#' @export
cellTypeToGenes = function(celltypeString, gotab, orgDb, cols=c("ENSEMBL", "SYMBOL"),...) {
 g = cellTypeToGO(celltypeString, gotab, ...)
 na.omit(AnnotationDbi::select(orgDb, keys=g$GOID, keytype="GO", columns=cols))
}

#' simple generation of children of 'choices' given as terms, returned as TermSet
#' @param choices vector of terms
#' @param ont instance of ontology_index (S3) from ontologyIndex package
#' @return TermSet instance
#' @examples
#' efoOnto = getEFOOnto()
#' secLevGen( "disease", efoOnto )
#' @export
secLevGen = function( choices, ont ) { 
   choices = ont$id[ match(choices, ont$name, nomatch=0 ) ]
   children_TAG( choices, ont )
}

