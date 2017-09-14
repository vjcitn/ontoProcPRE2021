
#countClasses = function(co) {
##
## crude iteration through ?->type->class triples,
## ignoring blank nodes
## check for primitives in redland?
##
# biq = new("Query", co@world, 
#   sprintf("SELECT ?a WHERE {?a %s %s}",
#     "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
#     "<http://www.w3.org/2002/07/owl#Class>"),
#       base_uri=NULL, query_language="sparql", query_uri=NULL)
#   xx = executeQuery(biq, co@model)
#   nc = 0
## handling of blank nodes needs to improve
#   while (!is.null(zz <- getNextResult(xx))) {
#     if (substr(zz, 1, 3) != "_:r") nc=nc+1
#     }
#   nc
#}

countTerms = function(ont) length(ont[[1]])
