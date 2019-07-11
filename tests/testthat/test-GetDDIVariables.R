context("GetDDIVariables")
load(
  system.file(
    "extdata/testdata/GetDDIVariables",
    "GetDDIVariablesData.RData",
    package = "bllflow"
  )
)

test_that("GetDDIVariables returns list of passed vars and their relative metadata", {
  testDDIVariables <- GetDDIVariables(TestEnvironment$`Test-1`$ddi, "age")
  expect_equal(testDDIVariables, TestEnvironment$`Test-1`$standardDDIVariables)
})
test_that("GetDDIVariables throws an error when invalid ddi is passed", {
  #Invalid ddi was passed please make sure to use the ReadDDI to import ddi
})
test_that("GetDDIVariables throws an error when invalid ddi is passed", {
  #DDI file is not present at specified path
})
test_that("GetDDIVariables throws a warning when not all passed vars are in ddi", {
  #Some of the passed vars were missing from ddi and were dropped from the return
})




