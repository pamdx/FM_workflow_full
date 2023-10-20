# Parameters
consolidated_imputed_data <- data_consolidated
dispatch_year <- end_year_FM_workflow_full
year <- c(start_year_FM_workflow_full:dispatch_year)
oc3 <- c("Aquaculture", "Marine Coastal Fishing", "Marine Deep-Sea Fishing", "Inland Waters Fishing", "Marine Fishing, nei", "Subsistence", "Unspecified", "Processing")
working_time <- c("Full time", "Part time", "Occasional", "Status Unspecified")
sex <- c("M", "F", "U")
country_ref_URL <- "https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_M49.csv"
format_vba <- TRUE