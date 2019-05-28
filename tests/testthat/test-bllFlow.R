context("bllFlow construction")
library(bllflow)

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

