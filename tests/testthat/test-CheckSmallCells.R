context("CheckSmallCells")
load(
  system.file(
    "extdata/testdata/CheckSmallCells",
    "CheckSmallCellsData.RData",
    package = "bllflow"
  )
)

test_that("CheckSmallCells Identifies small cells in tableOne", {
  checkedTableOne <- CheckSmallCells(TestEnvironment$`Test-1`$standardTableOne)
  expect_equal(checkedTableOne, TestEnvironment$`Test-1`$checkedTableOne)
})
test_that("CheckSmallCells Identifies small cells in longTable", {
  checkedLongTable <- CheckSmallCells(TestEnvironment$`Test-2`$standardLongTable)
  expect_equal(checkedLongTable, TestEnvironment$`Test-2`$checkedLongTable)
})
test_that("CheckSmallCells Identifies no small cells in tableOne", {
  # Should we display special message when no small cells are found
})
test_that("CheckSmallCells Identifies no small cells in longTable", {
  # Should we display special message when no small cells are found
})
test_that("CheckSmallCells throws an error when invalid tableOne is passed ", {
  #Invalid table one was passed please make sure ur using CreateTableOne to create it
})
test_that("CheckSmallCells throws an error when invalid longTable is passed ", {
  #Invalid longTable was passed please make sure ur using SummaryData to create it
})
