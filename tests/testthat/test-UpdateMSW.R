context("UpdateMSW")
load(
  system.file(
    "extdata/testdata/UpdateMSW",
    "UpdateMSWData.RData",
    package = "bllflow"
  )
)
test_that("UpdateMSW updates bllFlow variables", {
  bllFlowModel <- UpdateMSW(TestEnvironment$`Test-1`$testModel, newMSWVariables = TestEnvironment$`Test-1`$newVariables)
  expect_identical(bllFlowModel, TestEnvironment$`Test-1`$updatedTestModel)
})
test_that("UpdateMSW updates bllFlow variableDetails", {
  bllFlowModel <- UpdateMSW(TestEnvironment$`Test-2`$testModel, newMSWVariableDeatails = TestEnvironment$`Test-2`$newVariableDetails)
  expect_identical(bllFlowModel, TestEnvironment$`Test-2`$updatedTestModel)
})
test_that("UpdateMSW updates bllFlow ddi and populatedVariableDetails", {
  bllFlowModel <- UpdateMSW(TestEnvironment$`Test-3`$testModel, newDDI = TestEnvironment$`Test-3`$newDDI)
  expect_identical(bllFlowModel, TestEnvironment$`Test-3`$updatedTestModel)
})
test_that("UpdateMSW Throws a warning if no update parameters are passed", {
  # No update paramaters were passed no actions were taken
})
test_that("UpdateMSW Throws an error if invalid args are passed variables", {
  # The passed variables were invalid. Abandoning no updates were made
})
test_that("UpdateMSW Throws an error if invalid args are passed variableDetails", {
  # The passed variableDetails were invalid. Abandoning no updates were made
})
test_that("UpdateMSW Throws an error if invalid args are passed ddi", {
  # The passed ddi were invalid. Abandoning no updates were made
})
test_that("UpdateMSW Throws an error if invalid bllFlow is passed", {
  # The passed bllFlow object was invalid. Abandoning no updates were made
})


