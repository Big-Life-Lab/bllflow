#Test Environment generation for GetDDIVariables
TestEnvironment <- list()

# ---- Test for: GetDDIVariables returns list of passed vars and their relative metadata
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/GetDDIVariables/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  standardDDIVariables <- bllflow::GetDDIVariables(ddi, "age")
  
  TestEnvironment[["Test-1"]] <- list(standardDDIVariables = standardDDIVariables, ddi = ddi)
  
  return(TestEnvironment)
}

# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/GetDDIVariables/GetDDIVariablesData.RData")