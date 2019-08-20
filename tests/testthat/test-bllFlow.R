context("bllFlow construction")
load(system.file("extdata/testdata/BLLFlow",
                 "BLLFlowData.RData",
                 package = "bllflow"))

test_that("bllFlow can be passed no args", {
  model <- BLLFlow()
  expect_equal(model, TestEnvironment$`Test-1`$testModel)
})
test_that("bllFlow does not populate ddi related vars when no ddi is passed", {
  model <-
    BLLFlow(
      data = TestEnvironment$`Test-2`$data,
      variables =  TestEnvironment$`Test-2`$variables,
      variableDetails =  TestEnvironment$`Test-2`$variableDetails
    )
  expect_equal(model, TestEnvironment$`Test-2`$testModel)
})
test_that("bllFlow populates appropriate ddi related vars", {
  model <-
    BLLFlow(
      data = TestEnvironment$`Test-3`$data,
      variables =  TestEnvironment$`Test-3`$variables,
      variableDetails =  TestEnvironment$`Test-3`$variableDetails,
      ddi = TestEnvironment$`Test-3`$ddi
    )
  expect_equal(model, TestEnvironment$`Test-3`$testModel)
})
test_that("bllFlow handles passed args of wrong type", {
  skip("Not Yet implemented")
  # Display error wrong arg passed? should match the classes that are checked above
})
test_that("bllFlow handles passed args with wrong/innapropriate information",
          {
            skip("Not Yet implemented")
            # This is the check for proper columns being there not cell content/ unless u want it to check column data types
          })
test_that("bllFlow handles empty passed args", {
  skip("Not Yet implemented")
  # What kind of error should it throw if something is passed of right class but its just empty as in empty data frame
})
test_that("bllFlow display appropriate warnings when trying to overwrite rows",
          {
            skip("Not Yet implemented")
            # are the current warnings i display the ones we wish to stick with?
          })
