
#' tabulate the basic outcome of PBMC 3K tutorial of Seurat
#' @return a data.frame
#' @examples
#' seur3kTab()
#' @export
seur3kTab = function() 
  read.delim(system.file("extdata/pbmc3k_8grp.tsv", package="ontoProc"), stringsAsFactors=FALSE)

