
#### Update scripts from GitHub if needed ####

source("scripts_update.R")

#### ####

## 0 - LOAD GLOBAL PARAMETERS

source("./parameters/parameters_FM_workflow_full.R")

## 1 - NEW DATA PROCESSING AND COMPARISON WITH SENT QUESTIONNAIRE

source("./modules/new_data/FM_newdata.R")
browseURL(paste0(path_comparisons, i, "_", start_year,"-", end_year,".html")) # open new data report

## 2- IMPUTATION OF RECEIVED DATA

#### Paths to scripts and folders ####

input_directory <- "./inputs/imputation_data/"
output_directory <- "./outputs/imputation_outputs/"

path_data_import <- "./modules/imputation/data_import.R"
path_final_data_export_viz <- "./modules/imputation/final_data_export_viz.R"
path_functions <- "./modules/imputation/functions_FM_imputation.R"
path_imputation_aggregated <- "./modules/imputation/imputation_aggregated.R"
path_imputation_subseries <- "./modules/imputation/imputation_subseries.R"
path_initialization <- "./modules/imputation/initialization.R"
path_inputs_update <- "./modules/imputation/inputs_update.R"
path_processing_aggregated <- "./modules/imputation/processing_aggregated.R"
path_report_imputation <- "./modules/imputation/report_FM_imputation.Rmd"
path_subseries_analysis <- "./modules/imputation/subseries_analysis.R"
path_parameters_estimations <- "./parameters/parameters_estimations.R"
path_parameters_imputation <- "./parameters/parameters_FM_imputation.R"

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

## 3 - CONSOLIDATE IMPUTED DATA

source("./modules/consolidation/FM_consolidation.R")

## 4 - EXPORT IN QUESTIONNAIRE FORMAT

source("./modules/export/FM_questionnaire_export.R")
