context("GetDDIDescription")
load(
  system.file(
    "extdata/testdata/GetDDIDescription",
    "GetDDIDescriptionData.RData",
    package = "bllflow"
  )
)

test_that("GetDDIDescription returns proper named list format", {
  testDDIDescription <-
    GetDDIDescription(TestEnvironment$`Test-1`$ddi)
  expect_equal(testDDIDescription,
               TestEnvironment$`Test-1`$standardDDIDescription)
})
test_that("GetDDIDescription throws an error when invalid ddi is passed", {
  skip("Not Yet implemented")
  #Invalid ddi was passed please make sure to use the ReadDDI to import ddi
})
test_that(
  "GetDDIDescription throws an error when ddi is missing required data used in creating the return",
  {
    skip("Not Yet implemented")
    #DDI does not contain full Description <missing data> was dropped
  }
)
