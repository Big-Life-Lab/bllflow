#Test Environment generation for GetDDIDescription
TestEnvironment <- list()

# ---- Test for: GetDDIDescription returns proper named list format
SetUpTest1 <- function(TestEnvironment) {
  library(bllflow)
  ddi <-
    bllflow::ReadDDI(
      system.file(
        "extdata/testdata/GetDDIDescription/",
        "Test-1",
        package = "bllflow"
      ),
      "pbcDDI.xml"
    )
  standardDDIDescription <- bllflow::GetDDIDescription(ddi)
  
  TestEnvironment[["Test-1"]] <- list(standardDDIDescription = standardDDIDescription, ddi = ddi)
  
  return(TestEnvironment)
}

# ---- Run all the Setups ----
RunAllSetUps <- function(TestEnvironment) {
  TestEnvironment <- SetUpTest1(TestEnvironment)
  
  return(TestEnvironment)
}
# ---- Save the variables needed for testing ----
TestEnvironment <- RunAllSetUps(TestEnvironment = TestEnvironment)
save(TestEnvironment, file = "inst/extdata/testdata/GetDDIDescription/GetDDIDescriptionData.RData")