# [1] "cellTypeToGenes" "cellTypeToGO"    "children_TAG"    "getCellLineOnto"
# [5] "getCellOnto"     "getChebiLite"    "getEFOOnto"      "label_TAG"      
# [9] "secLevGen"       "show"            "siblings_TAG"   


library(ontoProc)

context("ontology processing")

test_that("siblings compute", {
  efoOnto = getEFOOnto()
  sibs = siblings_TAG( ontology = efoOnto )
  expect_true(is(sibs,"TermSet"))
})

test_that("cellTypeToGenes yields genes", {
  data(allGOterms)
  library(org.Hs.eg.db)
  cc = cellTypeToGenes("GABAergic neuron", allGOterms, org.Hs.eg.db)
  expect_true(nrow(cc)==3)
})

test_that("children_TAG works", {
  co = getCellOnto()
  chn = children_TAG("CL:0000540", co)
  expect_true(nrow(chn@cleanFrame)==34) 
})

test_that("onto generators work", {
  onts = c("getCellLineOnto", "getCellOnto", "getChebiLite", "getEFOOnto")
  oo = vapply(onts, function(x) class(get(x)()), character(1))
  expect_true(all(oo=="ontology_index"))
})
  
test_that("label_TAG works", {
  co = getCellOnto()
  chn = label_TAG("CL:0000540", co)
  expect_true(as.character(chn) == "neuron")
})

test_that("secLevGen works", {
  co = getCellOnto()
  chn = secLevGen("neuron", co)
  expect_true(nrow(chn@cleanFrame)==34)
})
  

test_that("siblings_TAG works", {
  co = getCellOnto()
  chn = siblings_TAG("CL:0000540", co)
  expect_true(nrow(chn@cleanFrame)==22)
})

test_that("concatenation works", {
  efoOnto = getEFOOnto()
  defsibs = siblings_TAG("EFO:1001209", efoOnto)
  conc = c(defsibs, defsibs)
  expect_true(length(conc@ontoTags)==8)
})

