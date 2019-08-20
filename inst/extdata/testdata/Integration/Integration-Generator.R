#Test Environment generation for Integration
TestEnvironment <- list()

# ---- Integration TEST ----


# ---- Test for: BllFlowObject can be created empty then populated
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-1",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/Integration/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <-
    bllflow::BLLFlow(
      data = pbc,
      variables = variables,
      variableDetails = variableDetails,
      ddi = ddi
    )
  TestEnvironment[["Test-1"]] <- list(testModel = testModel)
  
  return(TestEnvironment)
}
# ---- Test for: Long table can be made from a generated bllFlowObject
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-2",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/Integration/",
        "Test-2",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <-
    bllflow::BLLFlow(
      data = pbc,
      variables = variables,
      variableDetails = variableDetails,
      ddi = ddi
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  testTableOne <- bllflow::CreateTableOne(testModel)
  testSummaryTable <- bllflow::SummaryDataLong(testTableOne, bllFlowModel = testModel)
  
  TestEnvironment[["Test-2"]] <- list(testSummaryTable = testSummaryTable)
  
  return(TestEnvironment)
}
# ---- Test for: Small cells can be found in bllFlow generated table one as well as its long table
SetUpTest3 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-3",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-3",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/Integration/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <-
    bllflow::BLLFlow(
      data = pbc,
      variables = variables,
      variableDetails = variableDetails,
      ddi = ddi
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  testTableOne <- bllflow::CreateTableOne(testModel)
  testSummaryTable <- bllflow::SummaryDataLong(testTableOne, bllFlowModel = testModel)
  testTableOne <- bllflow::CheckSmallCells(testTableOne)
  testSummaryTable <- bllflow::CheckSmallCells(testSummaryTable)
  
  TestEnvironment[["Test-3"]] <- list(testSummaryTable = testSummaryTable, testTableOne = testTableOne)
  
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
save(TestEnvironment, file = "inst/extdata/testdata/Integration/IntegrationData.RData")