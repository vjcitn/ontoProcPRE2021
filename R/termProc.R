#' manage ontological data with tags and a DataFrame instance
#' @rdname TermSet-class
#' @importFrom Biobase selectSome
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors DataFrame
#' @return instance of TermSet
#' @exportClass TermSet
setClass("TermSet", representation(ontoURLs="character", 
   cleanFrame="DataFrame"))
#' validity method for TermSet
#' @rdname TermSet-class
#' @name validObject
#' @param object instance of TermSet
#' @examples
#' validObject(new("TermSet"))
#' @export
setValidity("TermSet", function(object) {
   chk1 = (length(object@ontoURLs)==nrow(object@cleanFrame))
   chk2 = all(names(object@cleanFrame) %in% c("clean", "url", "parent", "qualTerms"))
   report = ""
   if (!chk1) report = "ontoURLs length != nrow(cleanFrame)"
   if (!chk2) report = paste0(report, "...cleanFrame column names not as expected")
   if (report == "") return(TRUE)
   return(report)
})
#' abbreviated display for TermSet instances
#' @rdname TermSet-class
#' @aliases show
#' @aliases show,TermSet-method
#' @examples
#' if (!exists(".efosupp")) .efosupp = buildEFOOntSupport()
#' defsibs = siblings_URL( model=getModel(.efosupp), 
#'                world=getWorld(.efosupp))
#' class(defsibs)
#' defsibs
#' @export
setMethod("show", "TermSet", function(object) {
cat(sprintf("TermSet for %d terms\n", length(object@ontoURLs)))
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
  ansu = args[[1]]@ontoURLs
  ansf = args[[1]]@cleanFrame
  for (j in 2:length(args)) {
   ansu = c(ansu, args[[j]]@ontoURLs)
   ansf = rbind(ansf, args[[j]]@cleanFrame)
   }
 new("TermSet", ontoURLs = ansu, cleanFrame=ansf)
})

#' generate a TermSet with siblings of a given term
#' @import redland
#' @param urlstring a URI that serves as an rdf-schema class
#' @param model RDF model instance as defined in redland package
#' @param world RDF world instance as defined in redland package
#' @return TermSet instance
#' @examples
#' if (!exists(".efosupp")) .efosupp = buildEFOOntSupport()
#' siblings_URL( model=getModel(.efosupp), world=getWorld(.efosupp))
#' @export
siblings_URL = function(urlstring="<http://www.ebi.ac.uk/efo/EFO_1001209>", model,
   world) {
   parentQstr = sprintf("SELECT ?c WHERE {%s <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?c}", urlstring)
   query <- new("Query", world, parentQstr, base_uri=NULL, query_language="sparql", query_uri=NULL)
   q1 = executeQuery(query, model)
   if( length(grep("^_:r", chk <- getNextResult(q1)$c )>0) )
          chk <- getNextResult(q1)$c
   if( length(grep("^_:r", chk )>0) )  # check again
          chk <- getNextResult(q1)$c
   parent = chk
   subQstr = sprintf("SELECT ?a WHERE {?a <http://www.w3.org/2000/01/rdf-schema#subClassOf> %s }", parent)
   squery <- new("Query", world, subQstr, base_uri=NULL, query_language="sparql", query_uri=NULL)
   q2 = executeQuery(squery, model)
   res = NULL
   while(!is.null(ans <- getNextResult(q2))) {
       res = rbind(ans, res)
       }
   if (nrow(res)==0) return(NULL)
   allres = NULL
   for (i in 1:nrow(res)) {
      tstr = sprintf("SELECT ?c WHERE {%s <http://www.w3.org/2000/01/rdf-schema#label> ?c}",
             res[i,1])
      squery <- new("Query", world, tstr, base_uri=NULL, query_language="sparql", query_uri=NULL)
      q3 = executeQuery(squery, model)
      while (!is.null(ans <- getNextResult(q3))) {
         ans = c(res[i,1], ans, parent=parent)
         allres = rbind(ans, allres)
         }
      }
   clnTerms = getString(allres[,2])
   urls = as.character(unlist(allres[,1]))
   parent = as.character(unlist(allres[,3]))
   o = order(clnTerms)
   new("TermSet", ontoURLs=urls[o], cleanFrame=
          DataFrame(clean=clnTerms[o], url=urls[o], parent=parent[o], 
                qualTerms=as.character(allres[o,2])))
}

#' acquire the label of an ontology subject tag
#' @rdname siblings_URL
#' @aliases label_URL
#' @return character string
#' @examples
#' if (!exists(".efosupp")) .efosupp = buildEFOOntSupport()
#' label_URL( model=getModel(.efosupp), world=getWorld(.efosupp))
#' @export
label_URL = function(urlstring="<http://www.ebi.ac.uk/efo/EFO_0000311>", model, world) {
  tstr = sprintf("SELECT ?c WHERE {%s <http://www.w3.org/2000/01/rdf-schema#label> ?c}", urlstring)
  squery <- new("Query", world, tstr, base_uri=NULL, query_language="sparql", query_uri=NULL)
  q3 = executeQuery(squery, model)
  getNextResult(q3)
}

stripQual = function(x) gsub("(.*)\\^\\^.*", "\\1", x)
stripLang = function(x) gsub("(.*)@.*", "\\1", x)
getString = function(x) gsub("\\\"", "", stripQual(stripLang(x)))

#' acquire the label of an ontology subject tag
#' @rdname siblings_URL
#' @aliases children_URL
#' if (!exists(".efosupp")) .efosupp = buildEFOOntSupport()
#' children_URL( model=getModel(.efosupp), world=getWorld(.efosupp))
#' @return TermSet instance
#' @export
children_URL = function(urlstring="<http://www.ebi.ac.uk/efo/EFO_0000787>", model,
   world) {
   childQstr = sprintf("SELECT ?a WHERE {?a <http://www.w3.org/2000/01/rdf-schema#subClassOf> %s}", urlstring)
   query <- new("Query", world, childQstr, base_uri=NULL, query_language="sparql", query_uri=NULL)
   q1 = executeQuery(query, model)
   res = NULL
   while(!is.null(ans <- getNextResult(q1))) {
       res = rbind(ans, res)
       }
   if (nrow(res)==0) return(NULL)
   allurls = unlist(res)
   labs = sapply(allurls, function(x) label_URL(x, world=world, model=model))
   clnTerms = getString(labs)
   ac = as.character
   cbind(url=ac(allurls), labs=ac(labs))
   o = order(as.character(labs))
   new("TermSet", ontoURLs=allurls[o], cleanFrame=
          DataFrame(clean=clnTerms[o], url=allurls[o], parent=rep(urlstring, length(o)),
                qualTerms=as.character(labs[o])))
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
#' data(allGOterms)
#' library(org.Hs.eg.db)
#' head(cellTypeToGO("serotonergic neuron", allGOterms))
#' head(cellTypeToGenes("serotonergic neuron", allGOterms, org.Hs.eg.db))
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

