# General parameters

input_directory <- "./inputs/imputation_data/"
output_directory <- "./outputs/imputation_outputs/"

source(path_data_import) # Load data from inputs folder

country_input <- select_country(FM_raw, country_input)
OC2_input <- select_sector(FM_raw, country_input)
start_year <- start_year_FM_workflow_full
end_year <- end_year_FM_workflow_full

theme_set(theme_bw()) # set simple theme for charts
flags_official <- c(NA, "B", "I", "M", "P", "Q", "T", "R")
flag_FAOestimate <- "E"
flag_remove <- "X"