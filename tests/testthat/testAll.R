
library(ontoProc)

#export(buildCellOntSupport)
#export(buildEFOOntSupport)
#export(cellTypeToGO)
#export(cellTypeToGenes)
#export(children_URL)
#export(getModel)
#export(getWorld)
#export(label_URL)
#export(siblings_URL)
#export(tenXplore)
#exportClasses(TermSet)
#exportClasses(rrdfSupport)
#exportMethods(show)
#importFrom(Biobase,selectSome)

context("ontology processing")

test_that("siblings compute", {
  if (!exists(".efosupp")) .efosupp = buildEFOOntSupport()
  sibs = siblings_URL( model=getModel(.efosupp), world=getWorld(.efosupp))
  expect_true(class(sibs)=="TermSet")
})


