#Test Environment generation for UpdateMSW
TestEnvironment <- list()

# ---- UpdateMSW TEST ----


# ---- Test for: UpdateMSW updates bllFlow variables
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-1",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  newVariables <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-1",
        "PBC-variablesUpdateTest.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/UpdateMSW/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  updatedTestModel <- bllflow::BLLFlow(data = pbc, variables = newVariables, variableDetails = variableDetails, ddi = ddi)
  TestEnvironment[["Test-1"]] <- list(testModel = testModel, updatedTestModel = updatedTestModel, newVariables = newVariables)
  
  return(TestEnvironment)
}
# ---- Test for: UpdateMSW updates bllFlow variableDetails
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-2",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  newVariableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-2",
        "PBC-variableDetailsUpdateTest.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/UpdateMSW/",
        "Test-2",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  updatedTestModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = newVariableDetails, ddi = ddi)
  TestEnvironment[["Test-2"]] <- list(testModel = testModel, updatedTestModel = updatedTestModel, newVariableDetails = newVariableDetails)
  
  return(TestEnvironment)
}
# ---- Test for: UpdateMSW updates bllFlow ddi and populatedVariableDetails
SetUpTest3 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-3",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/UpdateMSW/Test-3",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/UpdateMSW/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  newDDI <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/UpdateMSW/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDIUpdateTest.xml"
    )
  
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  updatedTestModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = newDDI)
  TestEnvironment[["Test-3"]] <- list(testModel = testModel, updatedTestModel = updatedTestModel, newDDI = newDDI)
  
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
save(TestEnvironment, file = "inst/extdata/testdata/UpdateMSW/UpdateMSWData.RData")