#Test Environment generation for ReadDDI
TestEnvironment <- list()

# ---- Test for: ReadDDI creates a proper BLLFlow DDI object
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/ReadDDI/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  
  TestEnvironment[["Test-1"]] <- list(standardDDI = ddi)
  
  return(TestEnvironment)
}

# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/ReadDDI/ReadDDIData.RData")