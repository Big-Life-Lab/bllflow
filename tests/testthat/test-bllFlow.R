context("bllFlow construction")
library(bllflow)
library(survival)
data(pbc)
variablesSheet <- read.csv(system.file("extdata", "PBC-variables.csv", package = "bllflow"))
variableDetails <- read.csv(system.file("extdata", "PBC-variableDetails.csv", package = "bllflow"))
ddi <- ReadDDI(system.file("extdata", package = "bllflow"), "pbcDDI.xml")

test_that("bllFlow can be passed no args", {
  model <- BLLFlow()
  expect_is(model, "BLLFlow")
  expect_equal(model$data, NULL)
  expect_equal(model$variables, NULL)
  expect_equal(model$variableDetails, NULL)
  expect_equal(model$additionalDDIMetaData, NULL)
  expect_equal(model$populatedVariableDetails, NULL)
  expect_equal(model$ddi, NULL)
})
test_that("bllFlow does not populate ddi related vars when no ddi is passed", {
  model <- BLLFlow(pbc, variablesSheet, variableDetails)
  expect_is(model, "BLLFlow")
  expect_is(model$data, "data.frame")
  expect_is(model$variables, "data.frame")
  expect_is(model$variableDetails, "data.frame")
  expect_equal(model$additionalDDIMetaData, NULL)
  expect_equal(model$populatedVariableDetails, NULL)
  expect_equal(model$ddi, NULL)
})
test_that("bllFlow populates appropriate ddi related vars", {
  model <- BLLFlow(pbc, variablesSheet, variableDetails,ddi)
  expect_is(model, "BLLFlow")
  expect_is(model$data, "data.frame")
  expect_is(model$variables, "data.frame")
  expect_is(model$variableDetails, "data.frame")
  expect_is(model$additionalDDIMetaData, "list")
  expect_is(model$populatedVariableDetails, "data.frame")
  expect_is(model$ddi, "BLLFlowDDI")
})
test_that("bllFlow handles passed args of wrong type", {

})
test_that("bllFlow handles passed args with wrong/innapropriate information", {
  
})
test_that("bllFlow handles empty passed args", {
  
})
test_that("bllFlow display appropriate warnings when trying to overwrite rows", {
  
})
