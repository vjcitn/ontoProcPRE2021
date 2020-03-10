#' obtain graphNEL from ontology_plot instance of ontologyPlot
#' @import graph
#' @import Rgraphviz
#' @importFrom ontologyPlot onto_plot
#' @param x instance of S3 class ontology_plot
#' @return instance of S4 graphNEL class
#' @examples
#' requireNamespace("Rgraphviz")
#' requireNamespace("graph")
#' cl = getCellOnto()
#' cl3k = c("CL:0000492", "CL:0001054", "CL:0000236", "CL:0000625",
#'    "CL:0000576", "CL:0000623", "CL:0000451", "CL:0000556")
#' p3k = ontologyPlot::onto_plot(cl, cl3k)
#' gnel = make_graphNEL_from_ontology_plot(p3k)
#' gnel = improveNodes(gnel, cl)
#' graph::graph.par(list(nodes=list(shape="plaintext", cex=.8)))
#' gnel = Rgraphviz::layoutGraph(gnel)
#' Rgraphviz::renderGraph(gnel)
#' @export
make_graphNEL_from_ontology_plot <- function(x) {
        ont_graph <- new(
                "graphAM", 
                adjMat=x[["adjacency_matrix"]], 
                edgemode="directed"
        )   
        as(ont_graph, "graphNEL")
}

#' inject linefeeds for node names for graph, with textual
#' annotation from ontology 
#' @param g graphNEL instance
#' @param ont instance of ontology from ontologyIndex
#' @export
improveNodes = function(g,ont) {
 nn = paste(sub(" ", "\n", ont$name[nodes(g)]), "\n", nodes(g), sep="")
 nodes(g) = nn
 g
}

#' high-level use of graph/Rgraphviz for rendering ontology relations
#' @param ont instance of ontology from ontologyIndex
#' @param terms2use character vector
#' @param cex numeric(1) defaults to .8, supplied to Rgraphviz::graph.par
#' @param ... passed to onto_plot of ontologyPlot
#' @return graphNEL instance (invisibly)
#' @examples
#' cl = getCellOnto()
#' cl3k = c("CL:0000492", "CL:0001054", "CL:0000236", "CL:0000625",
#'    "CL:0000576", "CL:0000623", "CL:0000451", "CL:0000556")
#' onto_plot2(cl, cl3k)
#' @export
onto_plot2 = function(ont, terms2use, cex=.8, ...) {
  pl = ontologyPlot::onto_plot(ont, terms2use, ...)
  gnel = make_graphNEL_from_ontology_plot(pl)
  gnel = improveNodes(gnel, ont)
  graph.par(list(nodes=list(shape="plaintext", cex=cex)))
  gnel = layoutGraph(gnel)
  renderGraph(gnel)
  invisible(gnel)
}

