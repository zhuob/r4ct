module_02             <- new.env()
module_02$name        <- "User Manual"
module_02$id          <- "User"
module_02$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()

# module_02$comp_01$ui <- function(){
#   
#   fluidPage(
#     uiOutput("help_rmd"),
#     tags$br() 
#     
#   )
#   
# }
# 
# module_02$comp_01$server <- function(input, output, session, data){
# 
#   app_dir <- system.file("shiny-examples", "bayes-go-nogo", package = "shinyapps4clinicaltrial")
#   rmd_file <- paste0(app_dir, "/", "help.Rmd")
#   
#   output$help_rmd <- renderUI({
#     HTML(markdown::markdownToHTML(knitr::knit(rmd_file, quiet = TRUE)))
#   })
#   
# }


module_02$comp_01$ui <- function(){

  app_dir <- system.file("shiny-examples", "bayes-go-nogo", package = "shinyapps4clinicaltrial")
  rmd_file <- paste0(app_dir, "/", "help.Rmd")
  
  fluidPage(
    withMathJax(includeMarkdown(rmd_file)),
    tags$br()

  )

}

module_02$comp_01$server <- function(input, output, session, data){
  
}