context("CheckSmallCells")
load(
  system.file(
    "extdata/testdata/CheckSmallCells",
    "CheckSmallCellsData.RData",
    package = "bllflow"
  )
)

test_that("CheckSmallCells Identifies small cells in tableOne", {
  checkedTableOne <-
    CheckSmallCells(TestEnvironment$`Test-1`$standardTableOne)
  expect_equal(checkedTableOne, TestEnvironment$`Test-1`$checkedTableOne)
})
test_that("CheckSmallCells Identifies small cells in longTable", {
  checkedLongTable <-
    CheckSmallCells(TestEnvironment$`Test-2`$standardLongTable)
  expect_equal(checkedLongTable,
               TestEnvironment$`Test-2`$checkedLongTable)
})
test_that("CheckSmallCells Identifies no small cells in tableOne", {
  skip("Not Yet implemented")
  # Should we display special message when no small cells are found
})
test_that("CheckSmallCells Identifies no small cells in longTable", {
  skip("Not Yet implemented")
  # Should we display special message when no small cells are found
})
test_that("CheckSmallCells throws an error when invalid tableOne is passed ",
          {
            skip("Not Yet implemented")
            #Invalid table one was passed please make sure ur using CreateTableOne to create it
          })
test_that("CheckSmallCells throws an error when invalid longTable is passed ",
          {
            skip("Not Yet implemented")
            #Invalid longTable was passed please make sure ur using SummaryData to create it
          })
