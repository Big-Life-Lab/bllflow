context("Integration")
load(
  system.file(
    "extdata/testdata/Integration",
    "IntegrationData.RData",
    package = "bllflow"
  )
)

test_that("BllFlowObject can be created empty then populated", {
  library(survival)
  data(pbc)
  testModel <- bllflow::BLLFlow(data = pbc)
  variables <- read.csv(
    system.file(
      "extdata/testdata/Integration/Test-1",
      "PBC-variables.csv",
      package = "bllflow"
    )
  )
  testModel <- bllflow::UpdateMSW(bllModel = testModel, newMSWVariables = variables)
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/Integration/Test-1",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  testModel <- bllflow::UpdateMSW(bllModel = testModel, newMSWVariableDeatails = variableDetails)
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/Integration/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::UpdateMSW(bllModel = testModel, newDDI = ddi)
  expect_equal(testModel, TestEnvironment$`Test-1`$testModel)
})
test_that("Long table can be made from a generated bllFlowObject", {
  library(survival)
  data(pbc)
  variables <- read.csv(
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
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  testTableOne <- bllflow::CreateTableOne(testModel)
  testSummaryTable <- bllflow::SummaryDataLong(testTableOne, bllFlowModel = testModel)
  
  expect_equal(testSummaryTable, TestEnvironment$`Test-2`$testSummaryTable)
})
test_that("Small cells can be found in bllFlow generated table one as well as its long table", {
  library(survival)
  data(pbc)
  variables <- read.csv(
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
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  testTableOne <- bllflow::CreateTableOne(testModel)
  testSummaryTable <- bllflow::SummaryDataLong(testTableOne, bllFlowModel = testModel)
  testTableOne <- bllflow::CheckSmallCells(testTableOne)
  testSummaryTable <- bllflow::CheckSmallCells(testSummaryTable)
  
  expect_equal(testTableOne, TestEnvironment$`Test-3`$testTableOne)
  expect_equal(testSummaryTable, TestEnvironment$`Test-3`$testSummaryTable)
})





