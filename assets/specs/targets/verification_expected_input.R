input_one <- list(
  tar_target(
    create_depression_score_imputation_dataset,
    create_depression_score_imputation_dataset_function(
      data = data["study_dataset"],
      variables = role["create_depression_score_imputation_dataset"],
      survey_cycle_variable = role["survey_cycle"],
      survey_cycle_lower_limit = 2003,
      survey_cycle_upper_limit = 2014
    ),
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
    tar_target(
      merge_depression_score_imputed_dataset,
      merge_depression_score_imputed_dataset_function(
        depression_score_imputed_data = data["impute_depression_score"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    ),
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
    tar_target(
      merge_imputed_mood_disorder_data,
      merge_imputed_mood_disorder_data_function(
        mood_disorder_imputed_data = data["impute_mood_disorder"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    )
    
input_two <- list(
  tar_target(
    create_depression_score_imputation_dataset,
    create_depression_score_imputation_dataset_function(
      data = data["study_dataset"],
      variables = role["create_depression_score_imputation_dataset"],
      survey_cycle_variable = role["survey_cycle"],
      survey_cycle_lower_limit = 2003,
      survey_cycle_upper_limit = 2014
    ),
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
    tar_target(
      merge_depression_score_imputed_dataset,
      merge_depression_score_imputed_dataset_function(
        depression_score_imputed_data = data["impute_depression_score"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    ),
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
    tar_target(
      impute_mood_disorder,
      impute_mood_disorder_function(
        data = data["create_mood_disorder_imputation_dataset"],
        outcome = role["impute_mood_disorder_outcome"],
        predictors = role["impute_mood_disorder_predictors"],
        num_multiple_imputations = 	5,
        method = "logreg"
      )
    )
  )
      
input_three <- list(
  tar_target(
    create_depression_score_imputation_dataset,
    create_depression_score_imputation_dataset_function(
      data = data["study_dataset"],
      variables = role["create_depression_score_imputation_dataset"],
      survey_cycle_variable = role["survey_cycle"],
      survey_cycle_lower_limit = 2003,
      survey_cycle_upper_limit = 2014
    ),
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
    tar_target(
      merge_depression_score_imputed_dataset,
      merge_depression_score_imputed_dataset_function(
        depression_score_imputed_data = data["impute_depression_score"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
      )
    ),
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
    tar_target(
      merge_imputed_mood_disorder_data,
      merge_imputed_mood_disorder_data_function(
        mood_disorder_imputed_data = data["impute_mood_disorder"],
        study_dataset = data["study_dataset"],
        merge_by = role["id"]
       )
      ),
    tar_target(
      impute_mood_disorder,
      impute_mood_disorder_function(
        data = data["create_mood_disorder_imputation_dataset"],
        outcome = role["impute_mood_disorder_outcome"],
        predictors = role["impute_mood_disorder_predictors"],
        num_multiple_imputations = 	5,
        method = "logreg"
      )
    )
    
  )
  input_four <- list(
    tar_target(
      create_depression_score_imputation_dataset,
      create_depression_score_imputation_dataset_function(
        data = data["study_dataset"],
        variables = role["create_depression_score_imputation_dataset"],
        survey_cycle_variable = role["survey_cycle"],
        survey_cycle_lower_limit = 2003,
        wrong_name = 2014
      ),
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
      tar_target(
        merge_depression_score_imputed_dataset,
        merge_depression_score_imputed_dataset_function(
          depression_score_imputed_data = data["impute_depression_score"],
          study_dataset = data["study_dataset"],
          merge_by = role["id"]
        )
      ),
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
      tar_target(
        merge_imputed_mood_disorder_data,
        merge_imputed_mood_disorder_data_function(
          mood_disorder_imputed_data = data["impute_mood_disorder"],
          study_dataset = data["study_dataset"],
          merge_by = role["id"]
        )
      )
            