#### Update all scripts ####

urls <- c("https://raw.githubusercontent.com/pamdx/FM_newdata/main/main.R", 
          "https://raw.githubusercontent.com/pamdx/FM_newdata/main/functions.R",
          "https://raw.githubusercontent.com/pamdx/FM_newdata/main/report_newdata.Rmd",
          
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/data_import.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/final_data_export_viz.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/functions.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/imputation_aggregated.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/imputation_subseries.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/initialization.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/inputs_update.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/processing_aggregated.R",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/report_imputation.Rmd",
          "https://raw.githubusercontent.com/pamdx/emputator/master/modules/subseries_analysis.R",
          
          "https://raw.githubusercontent.com/pamdx/export_FM_questionnaire/main/script.R")

destinations <- c("./modules/new_data/main.R",
                  "./modules/new_data/functions.R",
                  "./modules/new_data/report_newdata.Rmd",
                  
                  "./modules/emputator/data_import.R",
                  "./modules/emputator/final_data_export_viz.R",
                  "./modules/emputator/functions.R",
                  "./modules/emputator/imputation_aggregated.R",
                  "./modules/emputator/imputation_subseries.R",
                  "./modules/emputator/initialization.R",
                  "./modules/emputator/inputs_update.R",
                  "./modules/emputator/processing_aggregated.R",
                  "./modules/emputator/report_imputation.Rmd",
                  "./modules/emputator/subseries_analysis.R",
                  
                  "./modules/export/script.R")

for(i in seq_along(urls)){
  download.file(urls[i], destinations[i], mode="wb")
}