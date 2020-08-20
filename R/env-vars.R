# Modifies the global variable for active config 
set_config_env_name <- function(config_env_name) {
  Sys.setenv(R_CONFIG_ACTIVE = config_env_name)
}