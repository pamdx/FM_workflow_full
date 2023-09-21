
#### Update scripts from GitHub if needed ####

# source("scripts_update.R")

#### ####

## 1 - NEW DATA PROCESSING AND COMPARISON WITH SENT QUESTIONNAIRE

source("./modules/new_data/main.R")

## 2- IMPUTATION OF RECEIVED DATA

#### Paths to scripts and folders ####

input_directory <- "./inputs/imputation_data/"
output_directory <- "./outputs/imputation_outputs/"

path_data_import <- "./modules/emputator/data_import.R"
path_final_data_export_viz <- "./modules/emputator/final_data_export_viz.R"
path_functions <- "./modules/emputator/functions.R"
path_imputation_aggregated <- "./modules/emputator/imputation_aggregated.R"
path_imputation_subseries <- "./modules/emputator/imputation_subseries.R"
path_initialization <- "./modules/emputator/initialization.R"
path_inputs_update <- "./modules/emputator/inputs_update.R"
path_processing_aggregated <- "./modules/emputator/processing_aggregated.R"
path_report_imputation <- "./modules/emputator/report_imputation.Rmd"
path_subseries_analysis <- "./modules/emputator/subseries_analysis.R"
path_parameters_estimations <- "./parameters_estimations.R"
path_parameters_emputator <- "./parameters_emputator.R"

#### ####

#### Update input data if needed ####

# source(path_inputs_update)

#### ####

# Initialize app

source(path_initialization)

# Load estimation parameters

source(path_parameters_estimations)

# Aggregated imputation

source(path_processing_aggregated)

source(path_imputation_aggregated)

# Subseries imputation

source(path_imputation_subseries)

# Final data export and report generation

source(path_final_data_export_viz)

## 3 - EXPORT IN QUESTIONNAIRE FORMAT

source("./modules/export/script.R")
