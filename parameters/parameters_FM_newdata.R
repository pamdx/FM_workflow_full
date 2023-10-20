# NEW DATA

file_received <- choose.files(default = "%userprofile%/downloads/*", caption = "Please select the questionnaire file",
                              multi = TRUE)

path_sent <- "./inputs/sent_questionnaires/"
path_received <- paste0(dirname(file_received), "/")
path_comparisons <- "./outputs/new_data_reports/"
path_export <- "./inputs/imputation_data/"

path_report_newdata <- "./modules/new_data/report_FM_newdata.Rmd"
path_functions <- "./modules/new_data/functions_FM_newdata.R"

start_year <- start_year_FM_workflow_full
end_year <- end_year_FM_workflow_full

data_sheet <- 5
data_range <- "A3:BG99" # MAKE SURE TO UPDATE

official_flags <- c(NA, "B", "E", "I", "M", "P", "Q", "T", "K") # List of official flags for data validation

country_names <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_M49.csv") %>%
  select(Name_En) %>%
  mutate(CountryUpper = toupper(Name_En))

files_received <- basename(file_received)
files_sent <- list.files(path = path_sent, full.names = FALSE, recursive = FALSE, ignore.case = TRUE) # List sent questionnaires
files_sent <- files_sent[grep(toupper(sub("\\_.*", "", files_received)), toupper(files_sent))]