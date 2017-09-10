#' container for RDF model and world instances as defined in redland
#' @rdname RrdfSupport-class
#' @aliases RrdfSupport-class
#' @aliases show,RrdfSupport-method
#' @aliases show
#' @importFrom methods new show
#' @exportClass RrdfSupport
setClass("RrdfSupport", representation(model="ANY", world="ANY"))
#' @rdname RrdfSupport-class
#' @param object instance of RrdfSupport
#' @export
setMethod("show", "RrdfSupport", function(object) {
  cat("RrdfSupport instance from ontoProc\n")
  cat(sprintf("there are %d class statements\n", countClasses(object)))
})

#' accessors for RrdfSupport
#' @export
getModel = function(x) x@model
#' @rdname getModel
#' @param x instance of RrdfSupport
#' @aliases getWorld
#' @export
getWorld = function(x) x@world

#' read and model the Experimental Factor Ontology as shipped in OWL and parsed in redland
#' @export
buildEFOOntSupport = function() {
 EFworld <- new("World")
 EFstorage <- new("Storage", EFworld, "hashes", name="", options="hash-type='memory'")
 EFmodel <- new("Model", world=EFworld, EFstorage, options="")
 EFparser <- new("Parser", EFworld)
 parseFileIntoModel(EFparser, EFworld, system.file("owl/efo.owl", package="ontoProc"), EFmodel)
 new("RrdfSupport", model=EFmodel, world=EFworld)
}

#' read and model the Cell Ontology as shipped in OWL and parsed in redland
#' @export
buildCellOntSupport = function() {
 CLworld <- new("World")
 CLstorage <- new("Storage", CLworld, "hashes", name="", options="hash-type='memory'")
 CLmodel <- new("Model", world=CLworld, CLstorage, options="")
 CLparser <- new("Parser", CLworld)
 parseFileIntoModel(CLparser, CLworld, system.file("owl/cl.owl", package="ontoProc"), CLmodel)
 new("RrdfSupport", model=CLmodel, world=CLworld)
}

#
# simple generation of children of 'choices'
#
secLevGen = function( TermSet, choices, rrdfsupp ) {
   inds = match(choices, TermSet@cleanFrame$clean)
   set = lapply(inds, function(x) 
     try(children_URL(TermSet@cleanFrame$url[x], model=getModel(rrdfsupp), world=getWorld(rrdfsupp)), silent=TRUE))
   chk = sapply(set, function(x) inherits(x, "try-error"))
   if (any(chk)) set = set[-which(chk)]
   if (length(set)==0) return(NULL)
   if (length(set)==1) return(set[[1]])
   do.call(c, set)
}

