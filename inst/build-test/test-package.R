# devtools::install_local("../beverage_1.2.2.tar.gz", dependencies = NA, upgrade = "never")
# devtools::install_git("https://gitlab-cfdamodelingandsimulation.devops.amgen.com/bzhuo/beverage.git", ref = "dev",
#                       credentials = git2r::cred_user_pass("bzhuo", getPass::getPass()))
remove.packages("shinyapps4clinicaltrial", lib="~/R/win-library/4.1")

path_to_pkg <- dirname(getwd())
pkg_name_version <- list.files(path_to_pkg, pattern = ".tar.gz")
local_pkg <- paste(path_to_pkg, tail(pkg_name_version, 1), sep = "/")

devtools::install_local(local_pkg, dependencies = TRUE, upgrade = "never")
# devtools::install_github("zhuob/R4ClinicalTrial")
shinyapps4clinicaltrial::launch_app(appname = "myapp1")
shinyapps4clinicaltrial::launch_app(appname = "mTPI2")
shinyapps4clinicaltrial::launch_app(appname = "reconstruct-survival-data")
shinyapps4clinicaltrial::launch_app(appname = "bayes-go-nogo")
View(shinyapps4clinicaltrial::click_data)

library(tidyverse)
