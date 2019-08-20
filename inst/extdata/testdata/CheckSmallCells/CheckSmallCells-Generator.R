#Test Environment generation for CheckSmallCells
TestEnvironment <- list()

# ---- Test for: CheckSmallCells Identifies small cells in tableOne
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt", "stage"), factorVars = catVars)
  checkedTableOne <- bllflow::CheckSmallCells(standardTableOne)
  
  TestEnvironment[["Test-1"]] <- list(standardTableOne = standardTableOne, checkedTableOne = checkedTableOne)
  
  return(TestEnvironment)
}
# ---- Test for: CheckSmallCells Identifies small cells in longTable
SetUpTest2 <- function(TestEnvironment) {
  library(bllflow)
  library(survival)
  data(pbc)
  catVars <- c("status", "trt", "ascites", "hepato", "spiders", "edema", "stage")
  standardTableOne <- CreateTableOne(data = pbc,strata = c("trt","stage"), factorVars = catVars)
  standardLongTable <- bllflow::SummaryDataLong(standardTableOne)
  checkedLongTable <- bllflow::CheckSmallCells(standardLongTable)
  
  TestEnvironment[["Test-2"]] <- list(standardLongTable = standardLongTable, checkedLongTable = checkedLongTable)
  
  return(TestEnvironment)
}
# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  TestEnvironment <- SetUpTest2(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/CheckSmallCells/CheckSmallCellsData.RData")