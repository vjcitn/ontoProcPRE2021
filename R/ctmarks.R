#' app to review molecular properties of cell types via cell ontology
#' @param cl an import of a Cell Ontology (or extended Cell Ontology) in ontology_index form
#' @note Prototype of harvesting of cell ontology by searching
#' has_part, has_plasma_membrane_part, intersection_of and allied
#' ontology relationships.  Uses shiny.  Can perform better if getPROnto() and getGeneOnto() values
#' are in .GlobalEnv as pr and go respectively.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter transmute left_join
#' @importFrom DT renderDataTable dataTableOutput
#' @return a data.frame with features for selected cell types
#' @export
ctmarks = function(cl) {
 cumu <- NULL
# require(shiny)
# require(dplyr)
# require(magrittr)
# require(ontoProc)
 if (!exists("pr")) {
  message("acquiring protein ontology")
  pr <<- getPROnto()
  }
 if (!exists("go")) {
  message("acquiring GO")
  go <<- getGeneOnto()
  }
 rp = recognizedPredicates()
 lens = lapply(rp, function(x) which(sapply(cl[[x]],length)>0))
 kp = unique(unlist(lapply(lens,names)))
 clClassNames = sort(cl$name[kp])
 clClassDF = data.frame(tag=names(clClassNames), 
    text=as.character(clClassNames), stringsAsFactors=FALSE)
 clCL = clClassDF %>% dplyr::filter(grepl("^CL", tag))
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(width=3,
    helpText("CL classes, limited to those for which presence or absence
of plasma membrane parts (or high or low plasma membrane amounts, expression, etc.) are indicated.  See ontoProc::recognizedPredicates() for full list"),
    selectInput("CLclasses", "CL classes", 
      choices = clCL$text, selected=clCL$text[1],
      multiple=FALSE),
    helpText("Visit the 'tags' tab to examine assertions about the selected cell type.  A data.frame is accumulated for all selections, and sent to the current session when the app is stopped."),
    actionButton("btnSend", "Stop app")
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("plot", plotOutput("deriv", height="900px")),
     tabPanel("tags", 
        helpText("The table generated here consists of information obtained about the query cell type by traversing the 'intersection_of' Cell Ontology elements associated with it.  When multiple distinct entries are present in the 'tag' and 'name' fields, the properties of the query cell type are asserted to be the intersection of the properties of the named additional cell types.  'SYMBOL' values are obtained for PR: entries that have a
EXACT PRO-short-label [PRO:DNx] annotation, and are missing for other entries."),
        DT::dataTableOutput("picks")),
     tabPanel("about", helpText("The intention of this app is to
navigate the Cell Ontology, and its formally linked companions Protein
Ontology and Gene Ontology, for developing views of molecular components
that distinguish types of cells.  Given a name of a cell
type to which a CL: tag corresponds, the program finds the
tag and the 'intersection_of', 'has/lacks_plasma_membrane_part',
'has_high/low_plasma_membrane_amount' elements of
the associated ontology annotation.  When these elements involve
PR: or GO: references, these are retrieved and reported.  This
process iterates over all the CL: references in the
intersection_of element for the input cell type.
The Cell Ontology content was taken from the OBO foundry,
format-version: 1.2, 2 data-version: releases/2018-07-07."
   ))
     )
   )
  )
 )
 server = function(input, output) {
  output$deriv = renderPlot({
   tag = names(cl$name[ which(cl$name == input$CLclasses) ] )
   anc = unique(c(cl$ancestors[[tag]], isachain(cl, tag))) # cl$ancestors[[tag]]
   anc = grep("^CL", anc, value=TRUE)
   drp1 = cl$ancestors["CL:0000548"][[1]] # excl ancestors of
   drp2 = cl$ancestors["CL:0000003"][[1]] # native and animal
   drp = union(drp1, drp2)
   #onto_plot(cl, intersect(setdiff(anc,drp),clCL$tag))
   onto_plot2(cl, setdiff(anc,drp))
   })
  output$picks = DT::renderDataTable({
    curtag = (clCL %>% filter(text == input$CLclasses))[["tag"]]
    ans = CLfeats(cl, curtag)
    cumu <<- rbind(cumu, ans)
    ans
  })
  observe({
            if(input$btnSend > 0)
               isolate({
                 stopApp(returnValue=0)
                      })  
           })  
 }
 runApp(list(ui=ui, server=server))
 cumu
}
