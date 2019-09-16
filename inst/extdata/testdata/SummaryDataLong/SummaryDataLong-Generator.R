#Test Environment generation for SummaryDataLong
TestEnvironment <- list()

# ---- Test for: SummaryDataLong converts tableOne into a long table
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt"), factorVars = catVars)
  standardTable <- bllflow::SummaryDataLong(standardTableOne)
  
  TestEnvironment[["Test-1"]] <- list(standardTable = standardTable, standardTableOne =  standardTableOne)
  
  return(TestEnvironment)
}
# ---- Test for: SummaryDataLong appends tableOne to passed long table
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt"), factorVars = catVars)
  standardTable <- bllflow::SummaryDataLong(standardTableOne)
  standardCompareTableOne <- CreateTableOne(data = pbc,strata = c("trt","stage"), factorVars = catVars)
  standardCompareTable <- bllflow::SummaryDataLong(tableOne = standardCompareTableOne, longTable = standardTable)
  
  TestEnvironment[["Test-2"]] <- list(standardTable = standardTable, standardCompareTableOne =  standardCompareTableOne, standardCompareTable =  standardCompareTable)
  
  return(TestEnvironment)
}
# ---- Test for: SummaryDataLong append tableOne to passed long table as well as labels from bllFlow object
SetUpTest3 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt"), factorVars = catVars)
  standardTable <- bllflow::SummaryDataLong(standardTableOne)
  standardCompareTableOne <- CreateTableOne(data = pbc,strata = c("trt","stage"), factorVars = catVars)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/SummaryDataLong/Test-3",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/SummaryDataLong/Test-3",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/SummaryDataLong/",
        "Test-3",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  standardCompareTable <- bllflow::SummaryDataLong(tableOne = standardCompareTableOne, longTable = standardTable, bllFlowModel = testModel)
  
  TestEnvironment[["Test-3"]] <- list(standardTable = standardTable, standardCompareTableOne =  standardCompareTableOne, standardCompareTable =  standardCompareTable, testModel = testModel)
  
  return(TestEnvironment)
}
# ---- Test for: SummaryDataLong converts tableOne into a long table with labels from bllFlow object
SetUpTest4 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt"), factorVars = catVars)
  variables <-
    read.csv(
      system.file(
        "extdata/testdata/SummaryDataLong/Test-4",
        "PBC-variables.csv",
        package = "bllflow"
      )
    )
  variableDetails <-
    read.csv(
      system.file(
        "extdata/testdata/SummaryDataLong/Test-4",
        "PBC-variableDetails.csv",
        package = "bllflow"
      )
    )
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/SummaryDataLong/",
        "Test-4",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  testModel <- bllflow::BLLFlow(data = pbc, variables = variables, variableDetails = variableDetails, ddi = ddi)
  standardTable <- bllflow::SummaryDataLong(standardTableOne, bllFlowModel = testModel)
  
  TestEnvironment[["Test-4"]] <- list(standardTable = standardTable, standardTableOne = standardTableOne, testModel = testModel)
  
  return(TestEnvironment)
}

# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  TestEnvironment <- SetUpTest2(TestEnvironment)
  TestEnvironment <- SetUpTest3(TestEnvironment)
  TestEnvironment <- SetUpTest4(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/SummaryDataLong/SummaryDataLongData.RData")