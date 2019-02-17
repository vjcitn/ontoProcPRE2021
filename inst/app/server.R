
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
 server = function(input, output) {
  output$deriv = renderPlot({
   tag = names(cl$name[ which(cl$name == input$CLclasses) ] )
   anc = unique(c(cl$ancestors[[tag]], ontoProc:::isachain(cl, tag))) # cl$ancestors[[tag]]
   anc = grep("^CL", anc, value=TRUE)
   drp1 = cl$ancestors["CL:0000548"][[1]] # excl ancestors of
   drp2 = cl$ancestors["CL:0000003"][[1]] # native and animal
   drp = union(drp1, drp2)
   #onto_plot(cl, intersect(setdiff(anc,drp),clCL$tag))
   onto_plot2(cl, setdiff(anc,drp))
   })
  output$picks = renderDataTable({
    curtag = (clCL %>% filter(text == input$CLclasses))[["tag"]]
    ans = CLfeats(cl, curtag)
    ans = cbind(query=curtag, ans)
    rownames(ans) = NULL
    cumu <<- rbind(cumu, ans)
    ans
  })
  output$recpred = renderPrint({
    ontoProc:::recognizedPredicates()
    })
  output$clvers = renderPrint({
    cat(attr(cl, "version"), sep="\n")
    })
  output$prvers = renderPrint({
    cat(attr(pr, "version"), sep="\n")
    })
  output$govers = renderPrint({
    cat(attr(go, "version"), sep="\n")
    })
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },  
       content = function(con) {
         write.csv(cumu, con)
       }   
     )   

  observe({
            if(input$btnSend > 0)
               isolate({
                 stopApp(returnValue=0)
                      })  
           })  
 }
