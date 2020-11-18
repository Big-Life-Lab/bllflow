# library(drake)
# library(osfr)
# library(cchsflow)
# library(bllflow)

# credentials <- config::get(file = "credentials.yml", use_parent = FALSE)
# osf_auth(credentials$phiat_PAT)
# 
# cchs2001_p <- osf_retrieve_file("https://osf.io/dcj9s/") %>% osf_download(path = "Data", conflicts = "overwrite")
# cchs2003_p <- osf_retrieve_file("https://osf.io/2v4nz/") %>% osf_download("Data", conflicts = "overwrite")
# cchs2005_p <- osf_retrieve_file("https://osf.io/f5dyw/") %>% osf_download("Data", conflicts = "overwrite")
# cchs2007_2008_p <- osf_retrieve_file("https://osf.io/fpj2b/") %>% osf_download("Data", conflicts = "overwrite")
# cchs2009_2010_p <- osf_retrieve_file("https://osf.io/jgw9f/") %>% osf_download("Data", conflicts = "overwrite")
# cchs2011_2012_p <- osf_retrieve_file("https://osf.io/unbjc/") %>% osf_download("Data", conflicts = "overwrite")
# cchs2013_2014_p <- osf_retrieve_file("https://osf.io/8uw4h/") %>% osf_download("Data", conflicts = "overwrite")
# 
# 
# phiat_object <- bllflow_config_init(config_env_name = "default")
# phiat_object <- bllflow_config_read_data(phiat_object)
# phiat_object <- bllflow_config_rec_data(phiat_object)
# phiat_object <- bllflow_config_combine_data(phiat_object)
ExcludeOnAge <- function(combinedData){
  rec_obj <- recipe(.~., data = combinedData$working_data)
  rec_obj <- rec_obj %>% step_filter(DHHGAGE_cont >= 20, skip = FALSE)
  
  return(rec_obj)
}
SetTaggedNA <- function(rec_obj){
  
  rec_obj <- rec_obj %>% step_apply_missing_tagged_na(all_predictors(), tag_type = "c")
  
  return(rec_obj)
}
Z_score<- function(rec_obj){
  
  rec_obj <- rec_obj %>% step_z_score(FVCDCAR,FVCDFRU,FVCDJUI, FVCDSAL, FVCDVEG, potato_add, potato_minus, append = TRUE)
  
  return(rec_obj)
}
ExcludeOnNAHUI <- function(rec_obj){
  variables <- populate_on_Role()
  rec_obj <- rec_obj %>% step_tagged_naomit(binge_drinker,HUIDCOG, HUIDEMO, HUIDHSI, HUIGDEX, HUIGHER, HUIGMOB, HUIGSPE, HUIGVIS, HUPDPAD, FVCDCAR, 	
                                            FVCDFRU, FVCDJUI, FVCDSAL, FVCDVEG, potato_add, potato_minus, tag_type = "a")
  return(rec_obj)
}
ExcludeMissingHUIC <- function(rec_obj){
  
  rec_obj <- rec_obj %>% step_tagged_naomit(binge_drinker,HUIDCOG, HUIDEMO, HUIDHSI, HUIGDEX, HUIGHER, HUIGMOB, HUIGSPE, HUIGVIS, HUPDPAD, FVCDCAR, 	
                                            FVCDFRU, FVCDJUI, FVCDSAL, FVCDVEG, potato_add, potato_minus, tag_type = "c")
  
  return(rec_obj)
}
ExcludeMissingHUIB <- function(rec_obj){
  
  rec_obj <- rec_obj %>% step_tagged_naomit(binge_drinker,HUIDCOG, HUIDEMO, HUIDHSI, HUIGDEX, HUIGHER, HUIGMOB, HUIGSPE, HUIGVIS, HUPDPAD, FVCDCAR, 	
                                            FVCDFRU, FVCDJUI, FVCDSAL, FVCDVEG, potato_add, potato_minus, tag_type = "b")
  
  return(rec_obj)
}
Bake <- function(rec_obj, phiat_object){
  
  phiat_object$previous_module_data <- phiat_object$working_data
  
  rec_obj <- prep(rec_obj)
  
  phiat_object$working_data <- bake(rec_obj, new_data = phiat_object$working_data)
  
  return(phiat_object)
}

plan <- drake_plan(
  InitializeObject = bllflow_config_init(config_env_name = "default"),
  readingData = bllflow_config_read_data(InitializeObject),
  recoding = bllflow_config_rec_data(readingData),
  combinedData = bllflow_config_combine_data(recoding),
  run_exclude = ExcludeOnAge(combinedData),
  run_z_score = Z_score(run_exclude),
  run_ExcludeOnNAHUI = ExcludeOnNAHUI(run_z_score),
  run_ExcludeMissingHUIC = ExcludeMissingHUIC(run_ExcludeOnNAHUI),
  run_ExcludeMissingHUIB = ExcludeMissingHUIB(run_ExcludeMissingHUIC),
  out = Bake(run_ExcludeMissingHUIB, combinedData)
)

plan

make(plan)
