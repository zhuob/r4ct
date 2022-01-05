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
usethis::use_test("mtpi2_fun")
usethis::use_test("farrington-manning")     
usethis::use_test("mtd-estimate")
#
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
              "readr"          = "2.0.2", 
              "tidyr"          = "1.0.0", # for use of pivot_longer
              "shiny"          = "1.7.1", 
              "ggplot2"        = "3.3.5", 
              "dplyr"          = "1.0.7", 
              "stringr"        = "1.4.0",
              "shinydashboard" = "0.7.2", 
              "tibble"         = "3.1.5",
              "lifecycle"      = "1.0.1",
              "Rdpack"         = "2.1.2"  # for reference
              )

import_packages <- setNames(lapply(sort(names(import_packages)), 
                      FUN = function(n) import_packages[[n]]), sort(names(import_packages)))

# for required packages
for(k in 1:length(import_packages)){
  usethis::use_package(names(import_packages)[k], type = "Imports", min_version = import_packages[[k]])
}

# for suggested packages
suggest_packages <- list(
        "survminer"          = "0.4.9", 
        "DT"                 = "0.20", 
        "markdown"           = "1.1",
        "testthat"           = "3.1.1",
        "knitr"              = "1.36"
)
suggest_packages <- setNames(lapply(sort(names(suggest_packages)), 
                                   FUN = function(n) suggest_packages[[n]]), sort(names(suggest_packages)))

for(k in 1:length(suggest_packages)){
  usethis::use_package(names(suggest_packages)[k], type = "Suggests", min_version = suggest_packages[[k]])
}

## add check state on README see https://stackoverflow.com/questions/63140363/how-to-add-r-cmd-check-state-on-readme-at-github
usethis::use_github_action_check_standard()
# https://www.r-bloggers.com/2017/06/how-to-add-code-coverage-codecov-to-your-r-package/
usethis::use_coverage(type = c("codecov"))
usethis::use_github_action("test-coverage")
covr::codecov(token = "72a9bba5-c860-41f7-919b-120efff6a776")
# for calculating code coverage see https://cran.r-project.org/web/packages/covr/readme/README.html
# and  https://docs.travis-ci.com/user/tutorial/
usethis::use_travis() 
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

## add vignette file
usethis::use_vignette("help")


## update versions -----------------------------------------------------------
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
pack <- "r4ct"
path <- find.package(pack)
if (file.exists(paste0(pack, ".pdf"))) {file.remove(paste0(pack, ".pdf"))}

# system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
system(paste(file.path(R.home("bin"), "R"), "CMD", "Rd2pdf", shQuote(path)))

