#' Find common ancestors
#'
#' Given a set of ontology terms, find their latest common ancestors based on the term hierarchy.
#'
#' @param ... One or more (possibly named) character vectors containing ontology terms.
#' @param g A \link{graph} object containing the hierarchy of all ontology terms.
#' @param remove.self Logical scalar indicating whether to ignore ancestors containing only a single term (themselves).
#' @param descriptions Named character vector containing plain-English descriptions for each term.
#' Names should be the term identifier while the values are the descriptions.
#'
#' @return A \linkS4class{DataFrame} where each row corresponds to a common ancestor term.
#' This contains the columns \code{number}, the number of descendent terms across all vectors in \code{...};
#' and \code{descendents}, a \linkS4class{List} of DataFrames containing the identities of the descendents.
#' It may also contain the column \code{description}, containing the description for each term.
#' 
#' @details
#' This function identifies all terms in \code{g} that are the latest common ancestor (LCA) of any subset of terms in \code{...}.
#' An LCA is one that has no children that have the exact same set of descendent terms in \code{...},
#' i.e., it is the most specific term for that set of observed descendents.
#' Knowing the LCA is useful for deciding how terms should be rolled up to broader definitions in downstream applications,
#' usually when the exact terms in \code{...} are too specific for practical use.
#'
#' The \code{descendents} DataFrame in each row of the output describes the descendents for each LCA,
#' stratified by their presence or absence in each entry of \code{...}.
#' This is particularly useful for seeing how different sets of terms would be aggregated into broader terms,
#' e.g., when harmonizing annotation from different datasets or studies.
#' Note that any names for \code{...} will be reflected in the columns of the DataFrame for each LCA. 
#'
#' @examples
#' co <- getCellOnto(useNew=TRUE)
#'
#' # TODO: wrap in utility function.
#' parents <- co$parents
#' self <- rep(names(parents), lengths(parents))
#' library(igraph)
#' g <- make_graph(rbind(unlist(parents), self))
#'
#' # Selecting random terms:
#' LCA <- ontoProc:::findCommonAncestors(A=sample(names(V(g)), 20),
#'    B=sample(names(V(g)), 20), g=g)
#'
#' LCA[1,]
#' LCA[1,"descendents"][[1]]
#' 
#' @export 
#' @author Aaron Lun
#' @importFrom igraph subcomponent
#' @importFrom S4Vectors DataFrame List
findCommonAncestors <- function(..., g, remove.self=TRUE, descriptions=NULL) {
    terms <- list(...)
    if (is.null(names(terms))) {
        names(terms) <- sprintf("set%i", seq_along(terms))
    }

    all.terms <- unique(unlist(terms))
    all.ancestors <- lapply(all.terms, subcomponent, graph=g, mode="in")
    all.ancestors <- lapply(all.ancestors, names)
    by.ancestor <- split(
        rep(all.terms, lengths(all.ancestors)),
        unlist(all.ancestors)
    )

    # Removing ancestor nodes with the same count as its children.
    available <- names(by.ancestor)
    for (i in available) {
        if (!i %in% names(by.ancestor)) {
            next
        }

        counts <- lengths(by.ancestor)
        cur.ancestors <- subcomponent(g, i, mode="in")
        cur.ancestors <- setdiff(names(cur.ancestors), i)
        drop <- cur.ancestors[counts[i]==counts[cur.ancestors]]
        by.ancestor <- by.ancestor[!names(by.ancestor) %in% drop]
    }

    if (remove.self) {
        by.ancestor <- by.ancestor[lengths(by.ancestor) > 1L]
    }
    by.ancestor <- by.ancestor[order(lengths(by.ancestor))] # most specific terms first.

    # Decorating the output.
    descendents <- list()
    for (i in names(by.ancestor)) {
        current <- by.ancestor[[i]]
        df <- DataFrame(row.names=current)
        if (!is.null(descriptions)) {
            df$description <- descriptions[current]
        }

        presence <- list()
        for (b in names(terms)) {
            presence[[b]] <- current %in% terms[[b]]
        }
        df <- cbind(df, do.call(DataFrame, presence))
        descendents[[i]] <- df
    }

    df <- DataFrame(row.names=names(descendents)) 
    if (!is.null(descriptions)) {
        df$description <- descriptions[rownames(df)]
    }

    descendents <- List(descendents)
    df$number <- nrow(descendents)
    df$descendents <- descendents

    df
}
