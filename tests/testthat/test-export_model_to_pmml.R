test_that("Correctly converts the variables and variable details files into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/1"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})

test_that("Correctly converts a dummy step file into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/2"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})

test_that("Correctly converts a centering step file into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/3"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})

test_that("Correctly converts a RCS step file into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/4"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})

test_that("Correctly converts an interactions step file into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/5"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})

test_that("Correctly converts a fine and gray step file into PMML", {
  test_dir <- "../../../assets/specs/model-csv-to-pmml/test-files/6"
  
  actual_pmml <- convert_model_export_to_pmml(
    paste(test_dir, "model-export.csv", sep = ""), 
    database = "cchs2001_p"
  )
  actual_pmml_str <- XML::toString.XMLNode(actual_pmml)
  
  expected_pmml_file_path <- paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml_lines <- readLines(
    expected_pmml_file_path,
    file.info(expected_pmml_file_path)$size
  )
  expected_pmml_string <- paste(expected_pmml, collapse = "\n")
  
  expect_equal(actual_pmml_str, expected_pmml_string)
})