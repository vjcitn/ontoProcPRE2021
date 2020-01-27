#' generate a selectInput control for an ontologyIndex slice
#' @rawNamespace import("shiny", except=c("dataTableOutput", "renderDataTable"))
#' @param onto ontologyIndex instance
#' @param term character(1) term used as basis for term list option set in the control
#' @param inputId character(1) for use in server
#' @param label character(1) for labeling in ui
#' @param type character(1) 'siblings' or 'children', relationship to 'term' that the options will satisfy
#' @param multiple logical(1) passed to \code{\link[shiny]{selectInput}}
#' @param \dots additional parameters passed to \code{\link[shiny]{selectInput}}
#' @return a \code{\link[shiny]{selectInput}} control
#' @examples
#' makeSelectInput
#' @export
makeSelectInput = function(onto, term, type="siblings",
    inputId, label, multiple=TRUE, ...) {
  stopifnot (term %in% onto$name)
  print(type)
  stopifnot (type %in% c("siblings", "children"))
  ind = match(term, onto$name)
  tag = onto$id[[ind]]
  if (type=="siblings") tset = siblings_TAG( tag, onto )
  else if (type=="children") tset = children_TAG( tag, onto )
  selectInput(inputId, paste0(label, ": ", term, " (", type, ")"), choices=unname(tset@cleanFrame$clean),
    multiple=multiple, ...)
}

#library(ontoProc)
#efo = getEFOOnto()
#cello = getCellOnto()
#celli = getCellLineOnto()

#' demonstrate the use of makeSelectInput
## @param onto ontologyIndex instance
## @param term character(1) term used as basis for term list option set in the control
## @param inputId character(1) for use in server
## @param label character(1) for labeling in ui
## @param type character(1) 'siblings' or 'children', relationship to 'term' that the options will satisfy
## @param multiple logical(1) passed to \code{\link[shiny]{selectInput}}
## @param \dots additional parameters passed to \code{\link[shiny]{selectInput}}
#' @return Run only for side effect of starting a shiny app.
#' @examples
#'   if (interactive()) {
#' require(shiny)
#' print(demoApp())
#' }
#' @export
demoApp = function() {
 cello = getCellOnto()
 cellineo = getCellLineOnto()
 uber = getUBERON_NE()
 efo = getEFOOnto()
 onts = list(Cell=cello, CellLine=cellineo, uberon_ne=uber, EFO=efo)
 starts = list(Cell="neuron", CellLine="neuron", uberon_ne="anatomical system", EFO="neuron")
 
 ui = fluidPage(
       sidebarLayout(
        sidebarPanel(
         selectInput("ontoChoice", "ontology", 
            choices=c("Cell", "EFO", "CellLine", "uberon_ne"),
            selected = "EFO"),
         selectInput("relToStart", "relation", 
            choices=c("siblings", "children"),
            selected = "siblings"),
#         makeSelectInput(onto=onto, term=term, type=type, 
#           inputId=inputId, label=label, multiple=multiple, ...),
         uiOutput("newSelInp")
        ),
        mainPanel(
         helpText("abc"),
         textOutput("def")
        )
       )
      )
 server = function(input, output) {
   output$def = renderText("def")
   output$newSelInp = renderUI({
         makeSelectInput(onto=onts[[input$ontoChoice]], 
           term=starts[input$ontoChoice], type=input$relToStart, 
           inputId="newsel", label="newsel", multiple=TRUE)
         })
 }
 shinyApp(ui=ui, server=server)
}

    
