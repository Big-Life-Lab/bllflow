context("GetDDIDescription")
library(bllflow)

test_that("GetDDIDescription returns proper named list format", {
  testDDIDescription <- GetDDIDescription(ddi)
  expect_is(testDDIDescription, "list")
  expect_is(testDDIDescription$docDscr, "list")
  expect_is(testDDIDescription$stdyDscr, "list")
  expect_is(testDDIDescription$fileDscr, "list")
})
test_that("GetDDIDescription throws an error when invalid ddi is passed", {
  #Invalid ddi was passed please make sure to use the ReadDDI to import ddi
})
test_that("GetDDIDescription throws an error when ddi is missing required data used in creating the return", {
  #DDI does not contain full Description <missing data> was dropped
})




