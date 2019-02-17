#' app to review molecular properties of cell types via cell ontology
#' @param cl an import of a Cell Ontology (or extended Cell Ontology) in ontology_index form
#' @note Prototype of harvesting of cell ontology by searching
#' has_part, has_plasma_membrane_part, intersection_of and allied
#' ontology relationships.  Uses shiny.  Can perform better if getPROnto() and getGeneOnto() values
#' are in .GlobalEnv as pr and go respectively.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter transmute left_join
#' @return a data.frame with features for selected cell types
#' @export
 cumu <- NULL
 require(shiny)
 require(dplyr)
 require(magrittr)
 require(ontoProc)
load("extcl.rda")
cl = extcl
 if (!exists("pr")) pr <<- getPROnto()
 if (!exists("go")) go <<- getGeneOnto()
 rp = recognizedPredicates()
 lens = lapply(rp, function(x) which(sapply(cl[[x]],length)>0))
 kp = unique(unlist(lapply(lens,names)))
 clClassNames = sort(cl$name[kp])
 clClassDF = data.frame(tag=names(clClassNames), 
    text=as.character(clClassNames), stringsAsFactors=FALSE)
 clCL = clClassDF %>% dplyr::filter(grepl("^CL", tag))
 nct = nrow(clCL)
 ttxt = sprintf("The ctmarks() app of Bioconductor::ontoProc provides information on %s cell types catalogued in Cell Ontology.  See the 'about' tab for additional details.", nct)
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(width=3,
    helpText(ttxt),
    selectInput("CLclasses", "CL classes", 
      choices = clCL$text, selected=clCL$text[1],
      multiple=FALSE),
    helpText("Visit the 'tags' tab to examine assertions about the selected cell type.  A data.frame is accumulated for all selections, and can be retrieved as CSV."),
    downloadButton("downloadData", "Download selections"),
    actionButton("btnSend", "Stop app")
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("plot", plotOutput("deriv", height="700px")),
     tabPanel("tags", 
        helpText("The table generated here consists of information obtained about the query cell type by traversing the 'intersection_of' Cell Ontology elements associated with it.  When multiple distinct entries are present in the 'tag' and 'name' fields, the properties of the query cell type are asserted to be the intersection of the properties of the named additional cell types.  'SYMBOL' values are obtained for PR: entries that have a
EXACT PRO-short-label [PRO:DNx] annotation, and are missing for other entries."),
        dataTableOutput("picks")),
     tabPanel("about",  
       helpText(h3("Overview.")),
       helpText("The intention of this app is to
navigate the Cell Ontology, and its formally linked companions Protein
Ontology and Gene Ontology, for developing views of molecular components
that distinguish types of cells.  Given a name of a cell
type to which a CL: tag corresponds, the program finds the
tag and inspects targets of relationships noted below.
When these elements involve
PR: or GO: references, these are retrieved and recorded as
contributors to the definition of the query cell type.  This
process iterates over all the CL: references in the
intersection_of and is_a elements for the query cell type.
The Cell Ontology content was taken from the OBO foundry,
format-version: 1.2, 2 data-version: releases/2018-07-07."
),
      helpText(h3("Relationships traversed and recorded.")),
      verbatimTextOutput("recpred"),
      helpText(h3("Cell Ontology version information.")),
      helpText("[Note that for this prototype app, the Cell Ontology content was extended with 10 CL:Xnn entries reflecting
information reported in Bakken et al., PMID 29322913, on a set of expression-defined interneurons.]"),
      verbatimTextOutput("clvers"),
      helpText(h3("Protein Ontology version information.")),
      verbatimTextOutput("prvers"),
      helpText(h3("Gene Ontology version information.")),
      verbatimTextOutput("govers")
      )
     )
   )
  )
 )
