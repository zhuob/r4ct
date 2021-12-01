# devtools::install_local("../beverage_1.2.2.tar.gz", dependencies = NA, upgrade = "never")
# devtools::install_git("https://gitlab-cfdamodelingandsimulation.devops.amgen.com/bzhuo/beverage.git", ref = "dev",
#                       credentials = git2r::cred_user_pass("bzhuo", getPass::getPass()))
remove.packages("shinyapps4clinicaltrial", lib="~/R/win-library/4.1")
devtools::install_local("C:/Users/bzhuo/Box Sync/projects/method_development/shinyapps4clinicaltrial_0.1.0.tar.gz", dependencies = TRUE, upgrade = "never")
shinyapps4clinicaltrial::launch_app(appname = "myapp")
shinyapps4clinicaltrial::launch_app(appname = "mTPI2")
