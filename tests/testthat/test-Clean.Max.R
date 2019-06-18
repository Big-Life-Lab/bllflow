context("Clean Max for BLLFlow")
load(
  system.file(
    "extdata/testdata/CleanMax",
    "CleanMaxData.RData",
    package = "bllflow"
  )
)

test_that("Clean Max Removes Max rows", {
  cleanedModel <- clean.Max(TestEnvironment$`Test-1`$testModel)
  expect_equal(cleanedModel, TestEnvironment$`Test-1`$cleanedModel)
})
test_that("Clean Max throws an error when invalid bllFlow is passed", {
  #Invalid bllFlowObject is passed please make sure you use BLLFlow to construct it
})
test_that("Clean Max throws an error when bllFlow is missing the variables MSW", {
  #VariablesMSW is missing from the bllFlow object update your object with variables MSW
})
test_that("Clean Max throws an error when bllFlow variables MSW is missing the Max column", {
  #VariablesMSW is missing the Max column therefore no operations were performed
})




