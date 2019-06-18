context("ReadDDI")
library(bllflow)
load(
  system.file(
    "extdata/testdata/ReadDDI",
    "ReadDDIData.RData",
    package = "bllflow"
  )
)

test_that("ReadDDI creates a proper BLLFlow DDI object", {
  #testDDI <- ReadDDI(system.file(    "extdata/testdata/ReadDDI",    "Test-1",package = "bllflow"),  "pbcDDI.xml")
  testDDI <- ReadDDI(system.file("extdata", "testdata", package = "bllflow"), "pbcDDI.xml")
  expect_equal(testDDI$variableMetaData$varlab, TestEnvironment$`Test-1`$standardDDI$variableMetaData$varlab)
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




