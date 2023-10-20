#### Update all scripts ####

urls <- c("https://raw.githubusercontent.com/pamdx/FM_newdata/main/FM_newdata.R", 
          "https://raw.githubusercontent.com/pamdx/FM_newdata/main/modules/functions_FM_newdata.R",
          "https://raw.githubusercontent.com/pamdx/FM_newdata/main/modules/report_FM_newdata.Rmd",
          
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/data_import.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/final_data_export_viz.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/functions_FM_imputation.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/imputation_aggregated.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/imputation_subseries.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/initialization.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/inputs_update.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/processing_aggregated.R",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/report_FM_imputation.Rmd",
          "https://raw.githubusercontent.com/pamdx/FM_imputation/master/modules/subseries_analysis.R",
          
          "https://raw.githubusercontent.com/pamdx/FM_consolidation/main/FM_consolidation.R",
          
          "https://raw.githubusercontent.com/pamdx/FM_questionnaire_export/main/FM_questionnaire_export.R")

destinations <- c("./modules/new_data/FM_newdata.R",
                  "./modules/new_data/functions_FM_newdata.R.R",
                  "./modules/new_data/report_FM_newdata.Rmd",
                  
                  "./modules/imputation/data_import.R",
                  "./modules/imputation/final_data_export_viz.R",
                  "./modules/imputation/functions_FM_imputation.R",
                  "./modules/imputation/imputation_aggregated.R",
                  "./modules/imputation/imputation_subseries.R",
                  "./modules/imputation/initialization.R",
                  "./modules/imputation/inputs_update.R",
                  "./modules/imputation/processing_aggregated.R",
                  "./modules/imputation/report_FM_imputation.Rmd",
                  "./modules/imputation/subseries_analysis.R",
                  
                  "./modules/consolidation/FM_consolidation.R",
                  
                  "./modules/export/FM_questionnaire_export.R")

for(i in seq_along(urls)){
  download.file(urls[i], destinations[i], mode="wb")
}