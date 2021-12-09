module_03             <- new.env()
module_03$name        <- "Step-by-Step Guide"
module_03$id          <- "User"
module_03$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_03$comp_01 <- new.env()

# module_03$comp_01$ui <- function(){
# 
# 
#   # first copy the REFERENCES.bib to the folder
#   bib_loc <- system.file("REFERENCES.bib", package = "shinyapps4clinicaltrial")
#   # find the location of .Rmd
#   app_dir <- system.file("shiny-examples", "reconstruct-survival-data", package = "shinyapps4clinicaltrial")
# 
#   if(!file.exists(paste0(app_dir, "/", "help.html"))){
#     # copy .bib file to generate help.html
#     if(!file.exists(paste0(app_dir, "/", "REFERENCES.bib"))){
#       file.copy(from = bib_loc, to = app_dir, overwrite = TRUE)
#     }
#     # generate html file
#     rmd_file <- paste0(app_dir, "/", "help.Rmd")
#     rmarkdown::render(input = rmd_file,  "all")
#   }
#   # include html file in the app
#   help_file <- paste0(app_dir, "/", "help.html")
# 
# 
#   includeHTML(help_file)
# 
# }
# 
# module_03$comp_01$server <- function(input, output, session, data){
# 
# }


module_03$comp_01$ui <- function(){

  app_dir <- system.file("shiny-examples", "reconstruct-survival-data", package = "shinyapps4clinicaltrial")
  help_file <- paste0(app_dir, "/", "help.html")


  includeHTML(help_file)

}

module_03$comp_01$server <- function(input, output, session, data){
  
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 02: The following chunck of code can evaluate the R code inside .Rmd file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# module_03$comp_01 <- new.env()
# 
# module_03$comp_01$ui <- function(){
#   
#   shinyUI({
#     fluidPage(
#       uiOutput("mdfile")
#     )
#   })
#   
# }
# 
# module_03$comp_01$server <- function(input, output, session, data){
#   
#   app_dir <- system.file("shiny-examples", "reconstruct-survival-data", package = "shinyapps4clinicaltrial")
#   rmd_file <- paste0(app_dir, "/", "help.Rmd")
#   print(rmd_file)
#   
#   output$mdfile  <- renderUI({
#     HTML(markdown::markdownToHTML(knitr::knit(rmd_file, quiet = TRUE)))
#   })
#   
# }
