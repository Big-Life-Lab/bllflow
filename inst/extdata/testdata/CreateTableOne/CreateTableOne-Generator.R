#Test Environment generation for CreateTableOne
TestEnvironment <- list()

# ---- Test for: CreateTableOne creates tableOne with just bllFlow using bllFlow specs
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-1",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/CreateTableOne/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  standardTableOne <- bllflow::CreateTableOne(testModel)
  
  TestEnvironment[["Test-1"]] <- list(testModel = testModel, standardTableOne = standardTableOne)
  
  return(TestEnvironment)
}
# ---- Test for: CreateTableOne creates tableOne with just passed vars that creates tableOne with just those vars
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-2",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/CreateTableOne/",
        "Test-2",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  standardVariables <- c("age","trt")
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  standardTableOne <- bllflow::CreateTableOne(testModel,vars = standardVariables)
  
  TestEnvironment[["Test-2"]] <- list(testModel = testModel, standardTableOne = standardTableOne, standardVariables = standardVariables)
  
  return(TestEnvironment)
}
# ---- Test for: CreateTableOne creates tableOne with passed strata
SetUpTest3 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-3",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/CreateTableOne/Test-3",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/CreateTableOne/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  standardStrata <- c("sex")
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  standardTableOne <- bllflow::CreateTableOne(testModel, strata = standardStrata)
  
  TestEnvironment[["Test-3"]] <- list(testModel = testModel, standardTableOne = standardTableOne, standardStrata = standardStrata)
  
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
save(TestEnvironment, file = "inst/extdata/testdata/CreateTableOne/CreateTableOneData.RData")