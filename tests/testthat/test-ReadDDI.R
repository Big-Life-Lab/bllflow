context("ReadDDI")
load(system.file("extdata/testdata/ReadDDI",
                 "ReadDDIData.RData",
                 package = "bllflow"))

test_that("ReadDDI creates a proper BLLFlow DDI object", {
  testDDI <-
    ReadDDI(
      system.file("extdata/testdata/ReadDDI",    "Test-1", package = "bllflow"),
      "pbcDDI.xml"
    )
  expect_equal(testDDI, TestEnvironment$`Test-1`$standardDDI)
})
test_that("ReadDDI throws an error if an invalid path is given", {
  skip("Not Yet implemented")
  #Invalid path detected Aborting
})
test_that("ReadDDI throws an error if the ddi file is missing", {
  skip("Not Yet implemented")
  #DDI file is not present at specified path
})
test_that("ReadDDI throws an error if the ddi file is inproper format", {
  skip("Not Yet implemented")
  #The specified DDI file is invalid format Abandoning
})
