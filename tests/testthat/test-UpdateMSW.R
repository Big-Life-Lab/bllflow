context("UpdateMSW")
library(bllflow)
library(survival)
data(pbc)
variables <- read.csv(system.file("extdata/testdata", "PBC-variables.csv", package = "bllflow"))
newVariables <- read.csv(system.file("extdata/testdata", "PBC-variablesUpdateTest.csv", package = "bllflow"))
variableDetails <- read.csv(system.file("extdata/testdata", "PBC-variableDetails.csv", package = "bllflow"))
newVariableDetails <- read.csv(system.file("extdata/testdata", "PBC-variableDetailsUpdateTest.csv", package = "bllflow"))
ddiVariableDetails <- read.csv(system.file("extdata/testdata", "ddiPopVariableDetails.csv", package = "bllflow"))
ddi <- ReadDDI(system.file("extdata/testdata", package = "bllflow"), "pbcDDI.xml")
newDDI <- ReadDDI(system.file("extdata/testdata", package = "bllflow"), "pbcDDIUpdateTest.xml")
bllFlowModel <- BLLFlow()

test_that("UpdateMSW updates bllFlow variables", {
  bllFlowModel <- BLLFlow(pbc,variables,variableDetails,ddi)
  expect_identical(bllFlowModel[[pkg.globals$argument.Variables]], variables)
  bllFlowModel <- UpdateMSW(bllFlowModel,newMSWVariables = newVariables)
  expect(!identical(variables, newVariables),"Two sets of dataframes being tested are equal making the test unable to test")
  expect_identical(bllFlowModel[[pkg.globals$argument.Variables]], newVariables)
})
test_that("UpdateMSW updates bllFlow variableDetails", {
  bllFlowModel <- BLLFlow(pbc,variables,variableDetails,ddi)
  expect_identical(bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]], variableDetails)
  bllFlowModel <- UpdateMSW(bllFlowModel,newMSWVariableDeatails =  newVariableDetails)
  expect(!identical(variableDetails, newVariableDetails),"Two sets of dataframes being tested are equal making the test unable to test")
  expect_identical(bllFlowModel[[pkg.globals$bllFlowContent.VariableDetails]], newVariableDetails)
})
test_that("UpdateMSW updates bllFlow ddi and populatedVariableDetails", {
  bllFlowModel <- BLLFlow(pbc,variables,variableDetails,ddi)
  expect_identical(bllFlowModel[[pkg.globals$bllFlowContent.DDI]], ddi)
  bllFlowModel <- UpdateMSW(bllFlowModel,newDDI  =  newDDI)
  expect(!identical(ddi, newDDI),"Two sets of dataframes being tested are equal making the test unable to test")
  expect_identical(bllFlowModel[[pkg.globals$bllFlowContent.DDI]], newDDI)
  expect_equal(bllFlowModel[[pkg.globals$bllFlowContent.PopulatedVariableDetails]], ddiVariableDetails)
  
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


