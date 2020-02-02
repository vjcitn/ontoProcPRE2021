#' connect ontological categories between related, annotated SummarizedExperiments
#' @importFrom graph adj addEdge nodes
#' @param ont an ontologyIndex ontology instance
#' @param se1 SummarizedExperiment instance with `label.ont` among colData columns
#' @param se2 SummarizedExperiment instance with `label.ont` among colData columns
#' @return a list with two sublists mapping from terms in one SE to descendant terms in the other SE
#' @export
connect_classes = function(ont, se1, se2) {
 # check that SE input has 'label.ont'
 stopifnot("label.ont" %in% names(colData(se1)))
 stopifnot("label.ont" %in% names(colData(se2)))
 t1 = se1$label.ont
 t2 = se2$label.ont
 # remove NA and duplicates
 acno = function(x) as.character(na.omit(x))
 t1 = acno(unique(t1))
 t2 = acno(unique(t2))
 # trace is_a relationships between classes in SEs
 t1d = lapply(t1, function(x) get_descendants(ont, x))
 names(t1d) = t1
 t2d = lapply(t2, function(x) get_descendants(ont, x))
 names(t2d) = t2
 # remove loops
 t1_desc_in_t2 = lapply(t1d, function(x) intersect(x, setdiff(t2,t1))) 
 t2_desc_in_t1 = lapply(t2d, function(x) intersect(x, setdiff(t1,t2))) 
 # remove isolated classes
 l1 = sapply(t1_desc_in_t2,length)
 l2 = sapply(t2_desc_in_t1,length)
 t1_desc_in_t2 = t1_desc_in_t2[l1>0]
 t2_desc_in_t1 = t2_desc_in_t1[l2>0]
 # build unidirectional graphs
 alln = unique(c(names(t1_desc_in_t2), unlist(t1_desc_in_t2),
          names(t2_desc_in_t1), unlist(t2_desc_in_t1)))
 g12 = new("graphNEL", nodes=alln, edgemode="directed")
 g21 = new("graphNEL", nodes=alln, edgemode="directed")
 z = lapply(names(t1_desc_in_t2), function(x) {g12 <<- addEdge(x, t1_desc_in_t2[[x]], g12)})
 z = lapply(names(t2_desc_in_t1), function(x) {g21 <<- addEdge(x, t2_desc_in_t1[[x]], g21)})
 # remove isolated nodes and return adjacency lists
 cln_adj = function(g) {
   a = adj(g, nodes(g))
   l = sapply(a, length)
   a[l>0]
   }
 # get names of input SEs
 n1 = deparse(substitute(se1))
 n2 = deparse(substitute(se2))
 ans = lapply(list(g12,g21), cln_adj)
 names(ans) = c(paste0(n1,"->",n2), paste0(n2,"->",n1))
 ans
}

#' use prose terminology with output of connect_classes
#' @param x a component of connect_classes output
#' @param cl an ontologyIndex ontology instance
#' @return a decorated list
#' @export
map2prose = function (x, cl) 
{
    names(x) = paste0(cl$name[names(x)], " (", names(x), ")")
    lapply(x, function(z) cl$name[z])
}


