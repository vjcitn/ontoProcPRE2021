# utility for caching recent obo for cell ontology
add_cache_cl_simple = function(cache = BiocFileCache::BiocFileCache(),
     target = "https://raw.githubusercontent.com/obophenotype/cell-ontology/master/cl-simple.obo") {
 BiocFileCache::bfcadd(cache, target)
}

#' load ontologies that may include non-ascii strings and therefore cannot be in data folder
#' @import BiocFileCache
#' @param useNew logical(1) only for getCellOnto if TRUE cell ontology of July 2018, otherwise use legacy
#' @param newest logical(1) if TRUE will use BiocFileCache to retrieve/use latest cl-simple.obo; overrides
#' @param cache instance of BiocFileCache
#' useNew
#' @examples
#' co = getCellOnto(useNew=TRUE)
#' co
#' clo = getCellLineOnto()
#' length(clo$id)
#' che = getChebiLite()
#' length(che$id)
#' efo = getEFOOnto()
#' length(efo$id)
#' @return instance of ontology_index (S3) from ontologyIndex
#' @note Provenance information is kept in the form
#' of excerpts of top records in `dir(system.file("obo", package="ontoProc"), full=TRUE)`
#' @export
getCellOnto = function(useNew=TRUE, newest=FALSE, cache=BiocFileCache::BiocFileCache())  {
    if (newest) {
     qu = BiocFileCache::bfcquery(cache, "cl-simple")
     if (nrow(qu) == 0) {
       chk = try(add_cache_cl_simple(cache=cache))
       if (inherits(chk, "try-error")) stop("could not add cl-simple.obo to cache")
       qu = BiocFileCache::bfcquery(cache, "cl-simple")
       }
     if (nrow(qu)>1) warning("more than one row mentions cl-simple, using first")
     return(ontologyIndex::get_OBO(qu$fpath[1], extract_tags="everything"))
    }
    sfstr = "ontoRda/cellOnto.rda"
    if (useNew) sfstr = "ontoRda/co_0718.rda"
    get(load(system.file(
      sfstr, package="ontoProc")))
    }


 


#' @rdname getCellOnto
#' @aliases getCellLineOnto
#' @export
getCellLineOnto = function() get(load(system.file(
      "ontoRda/cellLineOnto.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getEFOOnto
#' @export
getEFOOnto = function() get(load(system.file(
      "ontoRda/efoOnto.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getChebiLite
#' @export
getChebiLite = function() get(load(system.file(
      "ontoRda/chebi_lite.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getCellosaurusOnto
#' @export
getCellosaurusOnto = function() get(load(system.file(
      "ontoRda/cellosaurusOnto.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getUBERON_NE
#' @export
getUBERON_NE = function() get(load(system.file(
      "ontoRda/ne_uberon.rda", package="ontoProc")))

#' @rdname getCellOnto
#' @aliases getChebiOnto
#' @note getChebiOnto loads ontoRda/chebi_full.rda
#' @export
getChebiOnto = function() get(load(system.file(
      "ontoRda/chebi_full.rda", package="ontoProc")))

#' @rdname getCellOnto
#' @aliases getOncotreeOnto
#' @note getOncotreeOnto loads ontoRda/oncotree.rda
#' @return instance of ontology_index (S3) from ontologyIndex
#' @export
getOncotreeOnto = function() get(load(system.file(
      "ontoRda/oncotree.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getDiseaseOnto
#' @export
getDiseaseOnto = function() get(load(system.file(
      "ontoRda/diseaseOnto.rda", package="ontoProc")))
#' @rdname getCellOnto
#' @aliases getGeneOnto
#' @note getDiseaseOnto loads ontoRda/diseaseOnto.rda
#' @export
getGeneOnto = function() get(load(system.file(
      "ontoRda/goOnto.rda", package="ontoProc")))

#' @rdname getCellOnto
#' @aliases getHCAOnto
#' @note getHCAOnto loads ontoRda/hcaOnto.rda produced from hcao.owl at https://github.com/HumanCellAtlas/ontology/releases/tag/1.0.6 2/11/2019,
#' python pronto was used to convert OWL to OBO.
#' @export
getHCAOnto = function() get(load(system.file(
      "ontoRda/hcaOnto.rda", package="ontoProc")))

#' @rdname getCellOnto
#' @aliases getPROnto
#' @note getPROnto loads ontoRda/PRonto.rda, produced from http://purl.obolibrary.org/obo/pr.obo 'reasoned' ontology from OBO foundry, 02-08-2019.
#' In contrast to other ontologies, this is imported via get_OBO with
#' `extract_tags='minimal'`.
#' @export
getPROnto = function() get(load(system.file(
      "ontoRda/PROonto.rda", package="ontoProc")))


#' @rdname getCellOnto
#' @aliases getPATOnto
#' @note getPATOnto loads ontoRda/patoOnto.rda, produced from https://raw.githubusercontent.com/pato-ontology/pato/master/pato.obo from OBO foundry, 02-08-2019.
#' @export
getPATOnto = function() get(load(system.file(
      "ontoRda/patoOnto.rda", package="ontoProc")))
