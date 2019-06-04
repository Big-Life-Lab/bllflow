context("WriteDDI")
library(bllflow)
library(survival)
data(pbc)
variables <- read.csv(system.file("extdata/testdata", "PBC-variables.csv", package = "bllflow"))
variableDetails <- read.csv(system.file("extdata/testdata", "PBC-variableDetails.csv", package = "bllflow"))
ddi <- ReadDDI(system.file("extdata/testdata", package = "bllflow"), "pbcDDI.xml")

test_that("WriteDDI should create directory if one does not exist BLLFlow Version", {
  unlink("../testDataFolder", recursive = TRUE)
  pbcModel <- BLLFlow(pbc, variables, variableDetails, ddi)
  WriteDDIPopulatedMSW(pbcModel, "../testDataFolder", "newMSWvariableDetails.csv")
  expect(dir.exists("../testDataFolder"),"Dir does not exist")
  expect(file.exists("../testDataFolder/newMSWvariableDetails.csv"), "File does not exist")
  unlink("../testDataFolder", recursive = TRUE)
 
})
test_that("WriteDDI should create new file when writing BLLFlow Version", {
  dir.create("../testDataFolder")
  pbcModel <- BLLFlow(pbc, variables, variableDetails, ddi)
  WriteDDIPopulatedMSW(pbcModel, "../testDataFolder", "newMSWvariableDetails.csv")
  expect(dir.exists("../testDataFolder"),"Dir does not exist")
  expect(file.exists("../testDataFolder/newMSWvariableDetails.csv"), "File does not exist")
  unlink("../testDataFolder", recursive = TRUE)
})
test_that("WriteDDI should create new file when writing ddi Version", {
  dir.create("../testDataFolder")
  write.csv(variableDetails, "../testDataFolder/newMSWvariableDetails.csv")
  WriteDDIPopulatedMSW(ddi, "../testDataFolder", "newMSWvariableDetails.csv", "ddiCreatedVariableDetails.csv")
  expect(dir.exists("../testDataFolder"),"Dir does not exist")
  expect(file.exists("../testDataFolder/ddiCreatedVariableDetails.csv"), "File does not exist")
  unlink("../testDataFolder", recursive = TRUE)
})
test_that("writeDDI should Throw an error when no ddi is present for ddi version", {
  #What error should it throw i suggest No DDI file was found at <path> please make sure a valid DDI xml file is passed
})
test_that("WriteDDI should throw an error if passed bllFlow does not contain DDI", {
  #ERROR: Passed bllFlow Object does not contain ddi object. Make sure to update your bllFlow object with DDI
})
