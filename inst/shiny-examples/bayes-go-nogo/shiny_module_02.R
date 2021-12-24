module_02             <- new.env()
module_02$name        <- "Run Simulation"
module_02$id          <- "User"
module_02$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()


module_02$comp_01$ui <- function(){
  
  fluidPage(
    tabPanel("Operating Characteristics",
             sidebarPanel(
               numericInput("nsim", "Number of Simulations:", 10000,
                            min = NA, max = NA),
               textInput("orrtx",
                         "True ORRs for Simulations (separated by ','):",
                         value ="0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50"),
               actionButton("simulate", "Simulate!"),
               
               numericInput("case", "Which Case to Plot:", 1, min = 1, max = 100),
               sliderInput("lgdpos",
                           "Vertical Position of the Legend:",
                           min = 0,
                           max = 1,
                           value = 0.5),
               downloadButton('downloadData3', 'Download Plot (Go/NoGo Prob)'),
               downloadButton('downloadData4', 'Download Table')),
             
             mainPanel(
               tabsetPanel(type = "tabs", 
                           tabPanel("Go/NoGo Probabilities by Interim", 
                                    plotOutput("plot2")), 
                           tabPanel("Overall Operating Characteristics", 
                                    tableOutput("table2"), 
                                    tags$br()#, 
                                    
                                    # h3("Download Simulation Report"), 
                                    # downloadButton(outputId = "gng_report", class = "bright", icon = icon("cloud-download-alt")), 
                                    # radioButtons(inputId = "report_format", label = "Choose a Format", choices = c("HTML", "PDF", "WORD"), selected = "HTML")
                                    # 
                           ))
             )
    )
  )
  
}

module_02$comp_01$server <- function(input, output, session, data){
  
  output$downloadData3 <- downloadHandler(
    filename = "simplot.png",
    content = function(file) {
      res <- plotsim(psim(), input$px[1], input$px[2], input$case, input$lgdpos)
      ggplot2::ggsave(filename = file, plot = res, width = 10, height = 7)
      
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {paste('simtable.csv')},
    content = function(file) {
      psim2= tsim()
      readr::write_csv(psim2, file)
    }
  )
  
  output$plot2 <- renderPlot({
    plotsim(psim(), input$px[1], input$px[2], input$case, input$lgdpos)
  })
  
  
  
}