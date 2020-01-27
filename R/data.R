#' allGOterms: data.frame with ids and terms
#' @importFrom utils data read.delim
#' @docType data
#' @format data.frame instance
#' @source This is a snapshot of all the terms available from 
#' GO.db (3.4.2), August 2017, using keys(GO.db, keytype="TERM").
#' @examples
#' head(ontoProc::allGOterms)
"allGOterms"
#' stopWords: vector of stop words from xpo6.com
#' @docType data
#' @format character vector
#' @source \url{http://xpo6.com/list-of-english-stop-words/}
#' @note "Stop words" are english words that are assumed to contribute limited 
#' semantic value in the analysis of free text. 
#' @examples
#' data(stopWords)
#' head(stopWords)
"stopWords"
#' minicorpus: a vector of annotation strings found in 'study title' of SRA metadata.
#' @docType data
#' @format character vector
#' @source NCBI SRA
#' @note arbitrarily chosen from titles of RNA-seq studies for taxon 9606
#' @examples
#' data(minicorpus)
#' head(minicorpus)
"minicorpus"
#' humrna: a data.frame of SRA metadata related to RNA-seq in humans
#' @docType data
#' @format data.frame
#' @source NCBI SRA
#' @note arbitrarily chosen from RNA-seq studies for taxon 9606
#' @examples
#' data(humrna)
#' names(humrna)
#' head(humrna[,1:5])
"humrna"
#' PROSYM: HGNC symbol synonyms for PR (protein ontology) entries identified in Cell Ontology
#' @docType data
#' @format data.frame instance
#' @source OBO Foundry
#' @note This is a snapshot of the synonyms
#' component of an extract_tags='everything' import of PR.
#' The 'EXACT.*PRO-short.*:DNx' pattern is used to retrieve
#' HGNC symbols.
#' See ?getPROnto for more provenance information.
#' @examples
#' head(ontoProc::PROSYM)
"PROSYM"

#' packDesc2019: overview of ontoProc resources
#' @format data.frame instance
#' @note Brief survey of functions available to load serialized
#' ontology_index instances imported from OBO.
#' @examples
#' head(packDesc2019)
"packDesc2019"
