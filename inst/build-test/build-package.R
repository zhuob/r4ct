rm(list = ls())
library(devtools)
library(roxygen2)

## see reference here https://www.r-bloggers.com/2018/08/developing-r-packages-with-usethis-and-gitlab-ci-part-i/
################ add license ###################################################
#                                                                              #
usethis::use_mit_license("Bling Bling")            
usethis::use_vignette()

#
#                                                                              #
################### add test data ##############################################
usethis::use_data_raw("treatment-data")                                        # 
usethis::use_data(treatment)                                                   #  
# Since this data will be accessible to users of the package, it must be documented. 
usethis::use_r("click_data")    
usethis::use_r("risk_table")    
# Then add the documentation for the treatment data set to that script.        #   
#                                                                              #
################ add test cases ################################################
#  create testthat structure                                                   #
usethis::use_testthat()                                                        #
usethis::use_test("mtpi2_fun")                                                 #
#                                                                              #
#                                                                              #
#         The code above only needs to run once                                #
#                  at package initiation step                                  # 
################################################################################
usethis::use_roxygen_md()
usethis::use_lifecycle()

################### add dependency packages ####################################
usethis::use_pipe()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# add all of required packages that you are forcibly installing, here
#  Use NA for package version if you don't care what version is installed
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
import_packages <- list(
              "readr"          = NULL, 
              "tidyr"          = "1.0.0", # for use of pivot_longer
              "shiny"          = NULL, 
              "ggplot2"        = NULL, 
              "dplyr"          = NULL, 
              "stringr"        = NULL,
              "shinydashboard" = NULL, 
              "tibble"         = NULL, 
              "Rdpack"         = NULL  # for reference
              )

import_packages <- setNames(lapply(sort(names(import_packages)), 
                      FUN = function(n) import_packages[[n]]), sort(names(import_packages)))

# for required packages
for(k in 1:length(import_packages)){
  usethis::use_package(names(import_packages)[k], type = "Imports", min_version = import_packages[[k]])
}

# for suggested packages
suggest_packages <- list(
        "survminer"          = NULL, 
        "DT"                 = NULL, 
        "markdown"           = NULL, 
        "knitr"              = NULL
)
suggest_packages <- setNames(lapply(sort(names(suggest_packages)), 
                                   FUN = function(n) suggest_packages[[n]]), sort(names(suggest_packages)))

for(k in 1:length(suggest_packages)){
  usethis::use_package(names(suggest_packages)[k], type = "Suggests", min_version = suggest_packages[[k]])
}

## add check state on README see https://stackoverflow.com/questions/63140363/how-to-add-r-cmd-check-state-on-readme-at-github
usethis::use_github_action_check_standard()
# https://www.r-bloggers.com/2017/06/how-to-add-code-coverage-codecov-to-your-r-package/
usethis::use_coverage()
covr::codecov(token = "72a9bba5-c860-41f7-919b-120efff6a776")

usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

usethis::use_version("patch")



################ Check and test ################################################
devtools::load_all()
devtools::test()
# testthat::test_dir("tests/", reporter = "junit")
# check
devtools::document()
devtools::check()

# build
Sys.getenv("PATH")
#Sys.setenv(PATH = "C:/texlive/2016/bin/win32")
devtools::build(manual = T)

## generate the help manual.
pack <- "shinyapps4clinicaltrial"
path <- find.package(pack)
if (file.exists(paste0(pack, ".pdf"))) {file.remove(paste0(pack, ".pdf"))}

# system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
system(paste(file.path(R.home("bin"), "R"), "CMD", "Rd2pdf", shQuote(path)))

