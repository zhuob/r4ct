library(dplyr)
library(magrittr)
library(usethis)
library(readr)


if(!file.exists("data-raw/km-summary-table.csv")){
  
  current_km_data <- tibble::tibble(
    `NCT Number`                = "NCT00434642", 
    `Study Name`                = "OCEANS", 
    `Therapeutic Area`          = "Ovarian Cancer", 
    `Study Phase`               = "Phase 3", 
    `Data Type`                 = "PFS", 
    `Figure No. in Publication` = "Fig 2", 
    `Publication Link`          = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3646321/", 
    `Contributor`               = "NA", 
    `Email`                     = "NA"
  )
  write_csv(current_km_data, "data-raw/km-summary-table.csv")
  study_data <- readr::read_csv("data-raw/nct00434642_oceans_ovariancancer_phase3_pfs_fig2.csv")
  
} 


current_km_data <- readr::read_csv("data-raw/km-summary-table.csv")
usethis::use_data(current_km_data, overwrite = TRUE, compress = "xz", internal = TRUE)



if(!file.exists("data/click_data.rda")){
  click_data <- readr::read_csv("data-raw/data_all_arms.csv")
  usethis::use_data(click_data, compress = "xz")
}


if(!file.exists("data/risk_table.rda")){
  risk_table <- readr::read_csv("data-raw/data_nar.csv")
  usethis::use_data(risk_table, compress = "xz")
}




# 
# 
# load(file = "R/sysdata.rda")
# raw_data_name <- paste(current_km_data[, 1:6], sep = "_" , collapse = "_") %>% 
#   stringr::str_replace_all(pattern = "^[_]", replacement = "") %>% 
#   stringr::str_replace_all(pattern = " ", replacement = "") %>% tolower()
# 
# save(study_data, file = paste0(paste("data", raw_data_name, sep = "/"), ".rda"), compress = "xz")

