#' allGOterms: data.frame with ids and terms
#' @importFrom utils data
#' @docType data
#' @format data.frame instance
#' @source This is a snapshot of all the terms available from 
#' GO.db (3.4.2), August 2017, using keys(GO.db, keytype="TERM").
#' @examples
#' data(allGOterms)
#' head(allGOterms)
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
