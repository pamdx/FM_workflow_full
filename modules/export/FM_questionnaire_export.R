library(readr)
library(dplyr)
library(readxl)
library(tidyr)

# Parameters

source("./parameters/parameters_FM_questionnaire_export.R")

# Import data

mapping <- read_csv(country_ref_URL) %>%
  select(Corresp_Language, Name_En, Name_Es, Name_Fr, UN_Code) %>%
  mutate(Corresp_Language = case_when(
    Corresp_Language == "E" ~ "EN",
    Corresp_Language == "F" ~ "FR",
    Corresp_Language == "S" ~ "ES")
  )

# Check that all countries can be mapped to the official country list

if (any(!(unique(consolidated_imputed_data$geographic_area) %in% unique(mapping$Name_En)))) {
  
  wrong <- unique(consolidated_imputed_data$geographic_area)[which(!(unique(consolidated_imputed_data$geographic_area) %in% unique(mapping$Name_En)))]
  
  stop(paste0("The following countries could not be mapped to the official list of countries:\n - ", paste(wrong, collapse = "\n - ")))
  
}

# Transform data

consolidated_imputed_data$flag[consolidated_imputed_data$flag == "R"] <- ""

country <- unique(consolidated_imputed_data$geographic_area)

consolidated_imputed_data <- consolidated_imputed_data %>%
  filter(flag != "X" | is.na(flag)) %>%
  mutate(conc = paste(geographic_area, OC3, working_time, sex, year, sep = '_')) %>%
  mutate(value_conc = paste(value, flag, sep = '_')) %>%
  select(conc, value_conc)

# Generate forms in correct format

form <- expand.grid(country, oc3, working_time, sex, year) %>%
  as_tibble() %>%
  rename(country = Var1, oc3 = Var2, working_time = Var3, sex = Var4, year = Var5)

form <- form[order(match(form$country, country), match(form$oc3, oc3), match(form$working_time, working_time), match(form$sex, sex)),] %>%
  mutate(conc = paste(country, oc3, working_time, sex, year, sep = '_')) %>%
  left_join(consolidated_imputed_data, by = "conc") %>%
  select(-conc) %>%
  pivot_wider(names_from = year, values_from = value_conc)

for (column in year) {
  
  form <- form %>%
    separate(col = toString(column), into = c(paste0(column, "D"), paste0(column, "F")), sep = "_")
  
}

form[form == "NA"] = NA
  
if (format_vba) {

form <- form %>%
  left_join(mapping, by = c("country" = "Name_En"), keep = TRUE) %>%
  select(Name_En, Name_Fr, Name_Es, UN_Code, Corresp_Language, oc3:last_col()) %>%
  add_row(Name_En = "END")

for (i in colnames(form)[1:4]) {
 
  form[[i]][duplicated(form[[i]])] <- NA
   
}

form$Name_En <- gsub("/", "-", form$Name_En)

}

# Export data as csv

con <- file("./data_export_questionnaire.csv", encoding = "UTF-8")

write_csv(form, file = con, na = "")
