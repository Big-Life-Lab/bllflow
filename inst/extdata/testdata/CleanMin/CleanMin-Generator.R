#Test Environment generation for CleanMin
TestEnvironment <- list()

# ---- Test for: Clean Min Removes min rows
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/CleanMin/Test-1",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/CleanMin/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/CleanMin/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  cleanedModel <- bllflow::clean.Min(bllFlowModel = testModel)

  
  TestEnvironment[["Test-1"]] <- list(testModel = testModel, cleanedModel = cleanedModel)
  
  return(TestEnvironment)
}

# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/CleanMin/CleanMinData.RData")