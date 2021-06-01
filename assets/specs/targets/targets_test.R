context("Modules Test")
library(targets)
source("verification_expected_input.R")

test_that("Module verification returns TRUE when it matches modules.csv",{
  expect_true(verify_targets(targets_source = input_one, modules_path = "./modules_map.csv"))
})

test_that("Module verification returns appropriate error when a module step is missing",{
  expect_error((verify_targets(targets_source = input_two, modules_path = "./modules_map.csv"), "Missing step merge_imputed_mood_disorder_data")
})
  
test_that("Module verification returns appropriate error when module steps are out of order",{
  expect_error((verify_targets(targets_source = input_two, modules_path = "./modules_map.csv"), "Wrong order of steps")
})
  
test_that("Module verification returns appropriate error when module step contains wrong arguments",{
  expect_error((verify_targets(targets_source = input_two, modules_path = "./modules_map.csv"), "create_depression_score_imputation_dataset contains invalid step arguments")
})