#' list parentless nodes in ontology_index instance
#' @param x an ontology_index instance
#' @return a report (produced by cat()) of root ids and associated names
#' @examples
#' onto_roots
#' @export
 onto_roots <- function(x) {
    roots = x$id[ sapply(x$parents, length) == 0]
    show_roots <- roots[order(sapply(x$children[roots], length), 
        decreasing = TRUE)]
    cat(paste0(collapse = "", "\t", show_roots, " - ", x$name[show_roots], 
        "\n"), sep = "") 
}

