

#' as in Bakken et al. (2017 PMID 29322913) create gene signatures for k
#' cell types, each of which fails to express all but one gene in a set of k genes
#' @param idvec character vector of identifiers, must have names() set to identify cells bearing genes
#' @param conds character(2) tokens used to indicate condition to which signature element contributes
#' @param tags character vector of cell-type identifiers; for Cell Ontology use CL: as prefix,
#' one element for each element of idvec
#' @return a long data.frame
#' @examples
#' sigels = c("CL:X01"="GRIK3", "CL:X02"="NTNG1", "CL:X03"="BAGE2", 
#'         "CL:X04"="MC4R", "CL:X05"="PAX6", "CL:X06"="TSPAN12", "CL:X07"="hSHISA8", 
#'      "CL:X08"="SNCG", "CL:X09"="ARHGEF28", "CL:X10"="EGF")
#' sigdf = cyclicSigset(sigels)
#' head(sigdf)
#' @export
cyclicSigset = function(idvec, conds=c("hasExp", "lacksExp"), tags=paste0("CL:X", 1:length(idvec))) {
  combdf = expand.grid(idvec, idvec,
        stringsAsFactors=FALSE)
  todrp = which(apply(combdf,1,function(x)x[1]==x[2]))
  combdf = combdf[-todrp,]
  lacks = split(combdf[,1], combdf[,2])
  ins = unlist(lapply(split(combdf[,2], combdf[,2]), "[", 1))
  inds = rep(tags, each=length(idvec))
  typeid = names(idvec)
  names(typeid) = as.character(idvec)
  for (i in 1:length(lacks)) {
   lacks[[i]] = data.frame(gene=c(as.character(ins[i]), lacks[[i]]), type=typeid[ins[i]], stringsAsFactors=FALSE)
  }
  ans = do.call(rbind, lacks)
  ans = cbind(ans, cond=c(conds[1], rep(conds[2], length(idvec)-1)))
  rownames(ans) = NULL
  ans
}

# sigels = c("CL:X01"="GRIK3", "CL:X02"="NTNG1", "CL:X03"="BAGE2", 
#         "CL:X04"="MC4R", "CL:X05"="PAX6", "CL:X06"="TSPAN12", "CL:X07"="hSHISA8", 
#      "CL:X08"="SNCG", "CL:X09"="ARHGEF28", "CL:X10"="EGF")

#' use output of cyclicSigset to generate a series of character vectors constituting OBO terms
#' @param ldf a 'long format' data.frame as created by cyclicSigset
#' @param propmap a character vector with names of elements corresponding to 'abbreviated' relationship
#' tokens and element values corresponding to full relationship-naming strings
#' @param sigels a named character vector associating cell types (names) to genes expressed in a cyclic set,
#' one element per type
#' @param prologMaker a function with arguments (id, ...), in which id is character(1),
#' that generates a vector of strings that will be used for each cell type-specific term.
#' @note ldfToTerms is not sufficiently general to produce terms for any reasonably
#' populated long data frame/propmap combination, but it is a working example for the
#' cyclic set context.
#' @return a character vector, strings can be concatenated to OBO
#' @examples
#' # a set of cell types -- names are cell type token, values are genes expressed in a
#' # cyclic set -- each cell type expresses exactly one gene in the set and fails to
#' # express all the other genes in the set.  See Figs 3 and 4 of Bakken et al [PMID 29322913].
#' sigels = c("CL:X01"="GRIK3", "CL:X02"="NTNG1", "CL:X03"="BAGE2", 
#'         "CL:X04"="MC4R", "CL:X05"="PAX6", "CL:X06"="TSPAN12", "CL:X07"="hSHISA8", 
#'         "CL:X08"="SNCG", "CL:X09"="ARHGEF28", "CL:X10"="EGF")
#' # create the associated long data frame
#' ldf = cyclicSigset(sigels)
#' # describe the abbreviations
#' pmap = c("hasExp"="has_expression_of", lacksExp="lacks_expression_of")
#'
#' # now define the prolog for each cell type
#' makeIntnProlog = function(id, ...) {
#' # make type-specific prologs as key-value pairs
#'     c(
#'       sprintf("id: %s", id),
#'       sprintf("name: %s-expressing cortical layer 1 interneuron, human", ...),
#'       sprintf("def: '%s-expressing cortical layer 1 interneuron, human described via RNA-seq observations' [PMID 29322913]", ...),
#'       "is_a: CL:0000099 ! interneuron",
#'       "intersection_of: CL:0000099 ! interneuron")
#' }
#' tms = ldfToTerms(ldf, pmap, sigels, makeIntnProlog)
#' cat(tms[[1]], sep="\n")
#' @export
ldfToTerms = function(ldf, propmap, sigels, 
    prologMaker=function(id, ...) sprintf("id: %s", id) ) {
  tms = split(ldf, ldf$type)
  ctypes = names(tms)
  lapply(1:length(ctypes), function(x) ldfToTerm(tms[[x]], propmap=propmap,
                ctype=ctypes[x], gn=sigels[ctypes[x]], prologMaker))
}

#pmap = c("hasExp"="has_expression_of", lacksExp="lacks_expression_of")

ldfToTerm = function(ldf, propmap, ctype, gn, prologMaker) {
 nty = length(unique(ldf$type))
 stopifnot(nty==1)
 lins = paste(propmap[ldf$cond], ": ", 
    PROSYM[match(ldf$gene, PROSYM$SYMBOL), "PRID"], " ! ", ldf$gene, sep="")
 c("[Term]", prologMaker(ctype, gn), lins)
}

#aa = ldfToTerms(ldf, pmap, sigels)

#makeIntnProlog = function(id, ...) {
## make type-specific prologs as key-value pairs
#    c(
#      sprintf("id: %s", id),
#      sprintf("name: %s-expressing cortical layer 1 interneuron, human", ...),
#      sprintf("def: '%s-expressing cortical layer 1 interneuron, human described via RNA-seq observations' [PMID 29322913]", ...),
#      "is_a: CL:0000099 ! interneuron",
#      "intersection_of: CL:0000099 ! interneuron")
#}
#
#mm = makeProlog(names(sigels)[1], sigels[1])

#
#
#[Term]
#id: CL:X000001
#name: NTNG1-expressing cortical layer 1 interneuron, human
#def: 'cortical layer 1 interneuron described via RNA-seq observations' [PMID 29322913]
#is_a: CL:0000099 ! interneuron
#intersection_of: CL:0000099 ! interneuron
#has_expression_of: PR:000011467 ! NTNG1
#lacks_expression_of: PR:000008242 ! GRIK3
#lacks_expression_of: PR:000004625 ! BAGE2
#lacks_expression_of: PR:000001237 ! MC4R
#lacks_expression_of: PR:000012318 ! PAX6
#lacks_expression_of: PR:000016738 ! TSPAN12
#lacks_expression_of: PR:B8ZZ34 ! hSHISA8
#lacks_expression_of: PR:000015325 ! SNCG
#lacks_expression_of: PR:000013942 ! ARHGEF28
#lacks_expression_of: PR:000006928 ! EGF 
