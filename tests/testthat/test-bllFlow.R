context("bllFlow construction")
library(bllflow)
library(survival)
data(pbc)
variables <- read.csv(system.file("extdata/testdata", "PBC-variables.csv", package = "bllflow"))
variableDetails <- read.csv(system.file("extdata/testdata", "PBC-variableDetails.csv", package = "bllflow"))
ddi <- ReadDDI(system.file("extdata/testdata", package = "bllflow"), "pbcDDI.xml")

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
  model <- BLLFlow(pbc, variables, variableDetails)
  expect_is(model, "BLLFlow")
  expect_is(model$data, "data.frame")
  expect_is(model$variables, "data.frame")
  expect_is(model$variableDetails, "data.frame")
  expect_equal(model$additionalDDIMetaData, NULL)
  expect_equal(model$populatedVariableDetails, NULL)
  expect_equal(model$ddi, NULL)
})
test_that("bllFlow populates appropriate ddi related vars", {
  model <- BLLFlow(pbc, variables, variableDetails,ddi)
  expect_is(model, "BLLFlow")
  expect_is(model$data, "data.frame")
  expect_is(model$variables, "data.frame")
  expect_is(model$variableDetails, "data.frame")
  expect_is(model$additionalDDIMetaData, "list")
  expect_is(model$populatedVariableDetails, "data.frame")
  expect_is(model$ddi, "BLLFlowDDI")
})
test_that("bllFlow handles passed args of wrong type", {
# Display error wrong arg passed? should match the classes that are checked above
})
test_that("bllFlow handles passed args with wrong/innapropriate information", {
  # This is the check for proper columns being there not cell content/ unless u want it to check column data types
})
test_that("bllFlow handles empty passed args", {
  # What kind of error should it throw if something is passed of right class but its just empty as in empty data frame
})
test_that("bllFlow display appropriate warnings when trying to overwrite rows", {
  # are the current warnings i display the ones we wish to stick with?
})
