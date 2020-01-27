#' use Cell Ontology and Protein Ontology to identify cell-type defining conditions in which a given gene is named
#' @param sym gene symbol, must be used in protein ontology as a PRO:DNx exact match token
#' @param cl result of getCellOnto()
#' @param pr result of getPROnto()
#' @note Currently just checks for *plasma_membrane_part, *plasma_membrane_amount, and *Part conditions.
#' @return DataFrame if any hits are found.  A field 'cond' abbreviates the identified
#' conditions: (has/lacks)PMP (plasma membrane part) (hi/lo)PMAmt (plasma membrane amount), (has/lacks)Part.
#' @examples
#' if (!exists("cl")) cl = getCellOnto()
#' if (!exists("pr")) pr = getPROnto()
#' sym2CellOnto("ITGAM", cl, pr)
#' sym2CellOnto("FOXP3", cl, pr)
#' @export
sym2CellOnto = function(sym, cl, pr) {
 requireNamespace("ontoProc")
 requireNamespace("dplyr")
 requireNamespace("S4Vectors")
 stopifnot(length(sym)==1, inherits(cl, "ontology_index"),
    inherits(pr, "ontology_index"))
 lk = try((ontoProc::PROSYM %>% filter(SYMBOL == sym)))
 if (nrow(lk)==0) stop("can't resolve sym")
 prid = lk$PRID
 pmp = grep(prid, unlist(cl$has_plasma_membrane_part), value=TRUE)
 lmp = grep(prid, unlist(cl$lacks_plasma_membrane_part), value=TRUE)
 hpma = grep(prid, unlist(cl$has_high_plasma_membrane_amount), value=TRUE)
 lpma = grep(prid, unlist(cl$has_low_plasma_membrane_amount), value=TRUE)
 hpa = grep(prid, unlist(cl$has_part), value=TRUE)
 lpa = grep(prid, unlist(cl$lacks_part), value=TRUE)
#
# use of substr deals with the fact that unlist() will munge names.
# should use a filter on the lists instead...
#
 pmpdf = lmpdf = hpmadf = lpmadf = hpadf = lpadf = NULL
 if (length(pmp)>0) pmpdf = data.frame(sym=sym, cond="hasPMP", cl=substr(names(pmp),1,10), 
      type= cl$name[substr(names(pmp),1,10)])
 if (length(lmp)>0) lmpdf = data.frame(sym=sym, cond="lacksPMP", cl=substr(names(lmp),1,10), 
      type= cl$name[substr(names(lmp),1,10)])
 if (length(hpma)>0) hpmadf = data.frame(sym=sym, cond="hiPMAmt", cl=substr(names(hpma),1,10),
      type= cl$name[substr(names(hpma),1,10)])
 if (length(lpma)>0) lpmadf = data.frame(sym=sym, cond="loPMAmt", cl=substr(names(lpma),1,10),
      type= cl$name[substr(names(lpma),1,10)])
 if (length(hpa)>0) hpadf = data.frame(sym=sym, cond="hasPart", cl=substr(names(hpa),1,10),
      type= cl$name[substr(names(hpa),1,10)])
 if (length(lpa)>0) lpadf = data.frame(sym=sym, cond="lacksPart", cl=substr(names(lpa),1,10),
      type= cl$name[substr(names(lpa),1,10)])
 ans = DataFrame(do.call(rbind, list(pmpdf=pmpdf, lmpdf=lmpdf, hpmadf = hpmadf, lpmadf=lpmadf,
     hpadf=hpadf, lpadf=lpadf)))
 rownames(ans) = NULL
 ans
}
 
 
