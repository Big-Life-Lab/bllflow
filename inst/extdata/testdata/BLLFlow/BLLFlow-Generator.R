#Test Environment generation for BLLFlow
TestEnvironment <- list()

# ---- Test for: bllFlow can be passed no args
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  # Create empty bllFlow instance
  testModel <- bllflow::BLLFlow()
  
  TestEnvironment[["Test-1"]] <- list(testModel = testModel)
  
  return(TestEnvironment)
}
# ---- Test for: bllFlow does not populate ddi related vars when no ddi is passed
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  # Create non DDI populated bllFlow
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/BLLFlow/Test-2",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/BLLFlow/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails)
  
  TestEnvironment[["Test-2"]] <- list(testModel = testModel, data = pbc, variables = variables, variableDetails = variableDetails)
  
  return(TestEnvironment)
}
# ---- Test for: bllFlow populates appropriate ddi related vars
SetUpTest3 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/BLLFlow/Test-3",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/BLLFlow/Test-3",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/BLLFlow/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  
  TestEnvironment[["Test-3"]] <- list(testModel = testModel, data = pbc,variables = variables, variableDetails = variableDetails, ddi = ddi)
  
  return(TestEnvironment)
}
# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  TestEnvironment <- SetUpTest2(TestEnvironment)
  TestEnvironment <- SetUpTest3(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/BLLFlow/BLLFlowData.RData")