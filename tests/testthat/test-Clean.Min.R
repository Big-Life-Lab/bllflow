context("Clean Min for BLLFlow")
load(
  system.file(
    "extdata/testdata/CleanMin",
    "CleanMinData.RData",
    package = "bllflow"
  )
)

test_that("Clean Min Removes min rows", {
  cleanedModel <- clean.Min(TestEnvironment$`Test-1`$testModel)
  expect_equal(cleanedModel, TestEnvironment$`Test-1`$cleanedModel)
})
test_that("Clean Min throws an error when invalid bllFlow is passed", {
  #Invalid bllFlowObject is passed please make sure you use BLLFlow to construct it
})
test_that("Clean Min throws an error when bllFlow is missing the variables MSW", {
  #VariablesMSW is missing from the bllFlow object update your object with variables MSW
})
test_that("Clean Min throws an error when bllFlow variables MSW is missing the min column", {
  #VariablesMSW is missing the min column therefore no operations were performed
})




