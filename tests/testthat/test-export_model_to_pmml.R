expect_xml_equal <- function(test_dir, db_name, custom_function_files = NULL) {
  actual_pmml <- convert_model_export_to_pmml(
    test_dir,
    paste(test_dir, "model-export.csv", sep = ""),
    database_name = db_name,
    custom_function_files = custom_function_files
  )
  
  expected_pmml_file_path <-
    paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml <-
    XML::xmlTreeParse(expected_pmml_file_path)
  actual_pmml <- XML::asXMLNode(actual_pmml)
  
  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  suppressWarnings(expected_pmml_string <-
                     XML::toString.XMLNode(expected_pmml[[1]]$children$PMML))
  
  expect_equal(actual_pmml_string, expected_pmml_string)
}

test_that("Correctly converts the variables and variable details files into PMML",
{
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/1/"
  custom_function_files <- c(
    file.path(normalizePath(test_dir), "custom-function-file.R")
  )
  
  expect_xml_equal(test_dir, "cchs2001_p", custom_function_files)
  
})

test_that("Correctly converts a dummy step file into PMML", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/2/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a centering step file into PMML", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/3/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a RCS step file into PMML", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/4/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts an interactions step file into PMML", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/5/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a fine and gray step file into PMML", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/6/"
  custom_function_files <- c(
    file.path(normalizePath(test_dir), "custom-functions.R")
  )
  
  expect_xml_equal(test_dir, "cchs2001_p", custom_function_files)
})

test_that("Converting simple models", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/8/"
  custom_function_files <- c()
  
  expect_xml_equal(test_dir, "database_one", custom_function_files)
})

test_that("It correectly converts tables", {
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/9/"
  custom_function_files <- c()
  
  expect_xml_equal(test_dir, "database_one", custom_function_files)
})

describe("Interpolating values in the centerValue column", {
  it("7.1: It correctly interpolates values for the centerValue column in the centering step file", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.1/"
    custom_function_files <- c()
    
    expect_xml_equal(test_dir, "database_one", custom_function_files)  
  })
  
  it("7.2: It stops with an error if the interpolated value is not a nunber", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.2/"
    custom_function_files <- c()
    
    expect_error(
      convert_model_export_to_pmml(
        test_dir,
        paste(test_dir, "model-export.csv", sep = ""),
        database_name = "database_one",
        custom_function_files = c()
      ),
      "Error interpolating reference[reference$columnOne == 1, ]$value. Value a should be a number but is character and could not be coerced to a number",
      fixed = TRUE
    )
  })
  
  it("7.3: It stops with an error if no row was found in the reference file for the expression", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.3/"
    custom_function_files <- c()
    
    expect_error(
      convert_model_export_to_pmml(
        test_dir,
        paste(test_dir, "model-export.csv", sep = ""),
        database_name = "database_one",
        custom_function_files = c()
      ),
      "Error interpolating reference[reference$columnOne == 1, ]$value. No row found in reference file for expression.",
      fixed = TRUE
    )
  })
})