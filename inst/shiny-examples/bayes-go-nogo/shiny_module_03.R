module_03             <- new.env()
module_03$name        <- "Simulation Report"
module_03$id          <- "User"
module_03$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_03$comp_01 <- new.env()


module_03$comp_01$ui <- function(){

  # app_dir <- system.file("shiny-examples", "bayes-go-nogo", package = "shinyapps4clinicaltrial")
  # rmd_file <- paste0(app_dir, "/", "help.Rmd")
  
  fluidPage(
    
    h3("Download Simulation Report"), 
    downloadButton(outputId = "gng_report", class = "bright", icon = icon("cloud-download-alt")), 
    radioButtons(inputId = "report_format", label = "Choose a Format", choices = c("HTML", "PDF", "WORD"), selected = "HTML")
    
  )

}

module_03$comp_01$server <- function(input, output, session, data){
  
  
  output$gng_report <- downloadHandler(
    filename = function(){
      paste("gng-report", sep = ".",
            switch(input$report_format, PDF = "pdf", HTML = "html", WORD = "docx")
      )
    },
    
    content = function(file){
      src <- normalizePath("gng-report.Rmd")
      
      out_dir <- setwd(tempdir())
      on.exit(setwd(out_dir))
      file.copy(src, "gng-report.Rmd", overwrite = TRUE)
      
      out <- rmarkdown::render("gng-report.Rmd",
                               switch(input$report_format,
                                      PDF = rmarkdown::pdf_document(),
                                      HTML = rmarkdown::html_document(),
                                      WORD = rmarkdown::word_document()))
      
      file.rename(out, file)
    }
  )
}