rm(list = ls())
library(devtools)
library(roxygen2)

## create vignette
# devtools::use_vignette("my-vignette")
# devtools::use_data(CODES, internal = T)
## Create testthat
## only run once the following
# devtools::use_testthat()
## run test cases of functions
# options(testthat.output_file = "test-out.xml")
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
# devtools::install_local("../beverage_1.2.2.tar.gz", dependencies = NA, upgrade = "never")
# devtools::install_git("https://gitlab-cfdamodelingandsimulation.devops.amgen.com/bzhuo/beverage.git", ref = "dev",
#                       credentials = git2r::cred_user_pass("bzhuo", getPass::getPass()))
install.packages("C:/Users/bzhuo/Box Sync/projects/method_development/ShinyApp4CT_0.1.0.tar.gz", repos = NULL, type = "source")

## generate the help manual.
pack <- "ShinyApp4CT"
path <- find.package(pack)
if (file.exists(paste0(pack, ".pdf"))) {file.remove(paste0(pack, ".pdf"))}

# system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
system(paste(file.path(R.home("bin"), "R"), "CMD", "Rd2pdf", shQuote(path)))

