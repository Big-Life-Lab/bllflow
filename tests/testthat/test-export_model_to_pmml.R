expect_xml_equal <- function(test_dir, db_name) {
  actual_pmml <-
    convert_model_export_to_pmml(paste(test_dir, "model-export.csv", sep = ""),
                                 database_name = db_name)
  
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
            
            expect_xml_equal(test_dir, "cchs2001_p")
            
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
  
  expect_xml_equal(test_dir, "cchs2001_p")
})