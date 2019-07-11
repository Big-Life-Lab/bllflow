#Test Environment generation for WriteDDIPopulatedMSW
TestEnvironment <- list()

# ---- WriteDDIPopulatedMSW TEST ----


# ---- Test for: WriteDDI should create directory if one does not exist BLLFlow Version
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-1",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/",
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
  bllflow::WriteDDIPopulatedMSW(
    testModel,
    "inst/extdata/testdata/WriteDDIPopulatedMSW/Test-1",
    "standardWriteOutput.csv"
  )
  TestEnvironment[["Test-1"]] <- list(testModel = testModel)
  
  return(TestEnvironment)
}
# ---- Test for: WriteDDI should create new file when writing BLLFlow Version
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-2",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/",
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
  bllflow::WriteDDIPopulatedMSW(
    testModel,
    "inst/extdata/testdata/WriteDDIPopulatedMSW/Test-2",
    "standardWriteOutput.csv"
  )
  TestEnvironment[["Test-2"]] <- list(testModel = testModel)
  
  return(TestEnvironment)
}
# ---- Test for: WriteDDI should create new file when writing ddi Version
SetUpTest3 <- function(TestEnvironment) {
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-2",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  WriteDDIPopulatedMSW(
    ddi,
    "inst/extdata/testdata/WriteDDIPopulatedMSW/Test-3",
    "PBC-variableDetails.csv",
    "standardWriteOutput.csv"
  )
  TestEnvironment[["Test-3"]] <- list(ddi = ddi, variableDetails = variableDetails)
  
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
save(TestEnvironment, file = "inst/extdata/testdata/WriteDDIPopulatedMSW/WriteDDIPopulatedMSWData.RData")