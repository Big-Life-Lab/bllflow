context("WriteDDI")
load(
  system.file(
    "extdata/testdata/WriteDDIPopulatedMSW",
    "WriteDDIPopulatedMSWData.RData",
    package = "bllflow"
  )
)

test_that("WriteDDI should create directory if one does not exist BLLFlow Version",
          {
            unlink("../testDataFolder", recursive = TRUE)
            WriteDDIPopulatedMSW(
              TestEnvironment$`Test-1`$testModel,
              "../testDataFolder",
              "newMSWvariableDetails.csv"
            )
            expect(dir.exists("../testDataFolder"), "Dir does not exist")
            expect(
              file.exists("../testDataFolder/newMSWvariableDetails.csv"),
              "File does not exist"
            )
            expect_equal(
              read.csv("../testDataFolder/newMSWvariableDetails.csv"),
              read.csv(
                system.file(
                  "extdata/testdata/WriteDDIPopulatedMSW/Test-1",
                  "standardWriteOutput.csv",
                  package = "bllflow"
                )
              )
            )
            unlink("../testDataFolder", recursive = TRUE)
          })
test_that("WriteDDI should create new file when writing BLLFlow Version", {
  dir.create("../testDataFolder")
  WriteDDIPopulatedMSW(
    TestEnvironment$`Test-2`$testModel,
    "../testDataFolder",
    "newMSWvariableDetails.csv"
  )
  expect(dir.exists("../testDataFolder"), "Dir does not exist")
  expect(
    file.exists("../testDataFolder/newMSWvariableDetails.csv"),
    "File does not exist"
  )
  expect_equal(
    read.csv("../testDataFolder/newMSWvariableDetails.csv"),
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-2",
        "standardWriteOutput.csv",
        package = "bllflow"
      )
    )
  )
  unlink("../testDataFolder", recursive = TRUE)
})
test_that("WriteDDI should create new file when writing ddi Version", {
  dir.create("../testDataFolder")
  write.csv(
    TestEnvironment$`Test-3`$variableDetails,
    "../testDataFolder/newMSWvariableDetails.csv",
    row.names = FALSE
  )
  WriteDDIPopulatedMSW(
    TestEnvironment$`Test-3`$ddi,
    "../testDataFolder",
    "newMSWvariableDetails.csv",
    "ddiCreatedVariableDetails.csv"
  )
  expect(dir.exists("../testDataFolder"), "Dir does not exist")
  expect(
    file.exists("../testDataFolder/ddiCreatedVariableDetails.csv"),
    "File does not exist"
  )
  expect_equal(
    read.csv("../testDataFolder/ddiCreatedVariableDetails.csv"),
    read.csv(
      system.file(
        "extdata/testdata/WriteDDIPopulatedMSW/Test-3",
        "standardWriteOutput.csv",
        package = "bllflow"
      )
    )
  )
  unlink("../testDataFolder", recursive = TRUE)
})
# ----
test_that("writeDDI should Throw an error when no ddi is present for ddi version",
          {
            skip("Not Yet implemented")
            #What error should it throw i suggest No DDI file was found at <path> please make sure a valid DDI xml file is passed
          })
test_that("WriteDDI should throw an error if passed bllFlow does not contain DDI",
          {
            skip("Not Yet implemented")
            #ERROR: Passed bllFlow Object does not contain ddi object. Make sure to update your bllFlow object with DDI
          })
