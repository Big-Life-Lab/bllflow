context("CreateTableOne for BLLFlow")
load(
  system.file(
    "extdata/testdata/CreateTableOne",
    "CreateTableOneData.RData",
    package = "bllflow"
  )
)

test_that("CreateTableOne creates tableOne with just bllFlow using bllFlow specs", {
  testTableOne <- CreateTableOne(TestEnvironment$`Test-1`$testModel)
  expect_equal(testTableOne, TestEnvironment$`Test-1`$standardTableOne)
})
test_that("CreateTableOne creates tableOne with just passed vars that creates tableOne with just those vars", {
  testTableOne <- CreateTableOne(TestEnvironment$`Test-2`$testModel, vars = TestEnvironment$`Test-2`$standardVariables)
  expect_equal(testTableOne, TestEnvironment$`Test-2`$standardTableOne)
})
test_that("CreateTableOne creates tableOne with passed strata", {
  testTableOne <- CreateTableOne(TestEnvironment$`Test-3`$testModel, strata = TestEnvironment$`Test-3`$standardStrata)
  expect_equal(testTableOne, TestEnvironment$`Test-3`$standardTableOne)
})
test_that("CreateTableOne throws an error when data does not contain the strata", {
  #Passed data does not contain the passed strata. Operation Aborted
})
test_that("CreateTableOne throws a warning when data does not contain the passed vars", {
  #Either use the tableOne error or make our own???????????
})




