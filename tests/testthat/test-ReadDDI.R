context("ReadDDI")
library(bllflow)

test_that("ReadDDI creates a proper BLLFlow DDI object", {
  testDDI <- ReadDDI(system.file("extdata", "testdata", package = "bllflow"), "pbcDDI.xml")
  expect_is(testDDI, "BLLFlowDDI")
  expect_is(testDDI$variableMetaData, "list")
  expect_is(testDDI$ddiObject, "list")
})
test_that("ReadDDI throws an error if an invalid path is given", {
  #Invalid path detected Aborting
})
test_that("ReadDDI throws an error if the ddi file is missing", {
  #DDI file is not present at specified path
})
test_that("ReadDDI throws an error if the ddi file is inproper format", {
  #The specified DDI file is invalid format Abandoning
})




