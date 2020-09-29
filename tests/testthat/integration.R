library("XML")

test_that("The PMML file is correctly generated", {
  sample_cox_filename <- "../../assets/tests/integration/samplecox.rda"
  sample_cox_file <- readChar(
    sample_cox_filename,
    file.info(sample_cox_filename)$size
  )
  
  expect_equal(expected_output, sample_cox_file)
})
