library(targets)
library(huiport) # The package containing functions found in depression_imputation_module
Hui_impute <- create_targets_tepmlate()

list(
  # Create the dataset with which we will impute the depression score variable. Only include the survey cycles from 2003 to 2014 since mood disorder is one of the strongest predictors of depression score and it was only available during these cycles in the PUMF
  tar_target(
    create_depression_score_imputation_dataset,
    create_depression_score_imputation_dataset_function(
      data = data["study_dataset"],
      variables = role["create_depression_score_imputation_dataset"],
      survey_cycle_variable = role["survey_cycle"], 
      survey_cycle_lower_limit = 2003,
      survey_cycle_upper_limit = 2014
    ),
    # Imputes the depression score variables using the MICE method. Use a polytomous logistic regression method since there are multiple categories in the depression score variable.
    tar_target(
      impute_depression_score,
      impute_depression_score_function(
        data = create_depression_score_imputation_dataset,
        outcome = role["impute_depression_score_outcome"],
        predictors = role["impute_depression_score_predictors"],
        num_multiple_imputations = 5,
        method = "polr"
      )
    ),
    # Merge the depression score imputed dataset back into the original study dataset using the id column.
    tar_target(
      merge_depression_score_imputed_dataset,
      merge_depression_score_imputed_dataset_function(
        depression_score_imputed_data = data["impute_depression_score"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    ),
    # Create the dataset with which we will impute the mood disorder variable. Include all the cycles we have, which is everything from 2001 to 2014.
    tar_target(
      create_mood_disorder_imputation_dataset,
      create_mood_disorder_imputation_dataset_function(
        data = data["study_dataset"],
        variables = 	role["create_mood_disorder_imputation_dataset"],
        survey_cycle_variable = role["survey_cycle"],
        survey_cycle_lower_limit = 2001,
        survey_cycle_upper_limit = 2014
      )
    ),
    # Impute the mood disorder variable using MICE method with 5 iterations. Use the logsitc regression model since mood disorder has only 2 categories, Yes and No.
    tar_target(
      impute_mood_disorder,
      impute_mood_disorder_function(
        data = data["create_mood_disorder_imputation_dataset"],
        outcome = role["impute_mood_disorder_outcome"],
        predictors = role["impute_mood_disorder_predictors"],
        num_multiple_imputations = 	5,
        method = "logreg"
      )
    ),
    # Merge the mood disorder imputed dataset back into the original study dataset using the id column.
    tar_target(
      merge_imputed_mood_disorder_data,
      merge_imputed_mood_disorder_data_function(
        mood_disorder_imputed_data = data["impute_mood_disorder"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    )