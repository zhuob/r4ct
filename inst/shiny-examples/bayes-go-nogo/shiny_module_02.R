module_02             <- new.env()
module_02$name        <- "Run Simulation"
module_02$id          <- "User"
module_02$icon        <- "chart-area"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()


module_02$comp_01$ui <- function(){
  
  fluidPage(
            
    fluidRow(
      column(6, 
             numericInput("nsim", "Number of Simulations:", 10000,
                          min = NA, max = NA)), 
      column(6,     
             textInput("orrtx", "True ORRs for Simulations (separated by ','):",
                       value ="0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50")
             )
    ),   
               
    actionButton("simulate", "Simulate!", icon = icon("arrow-alt-circle-right"), class = "btn-info"),
    
    tags$br(),
    tableOutput("table2"),
    
    tags$br(),
    fluidRow(
      column(6,     selectInput("case", "Which Case to Plot:", choices = NULL)),
      column(6,     sliderInput("lgdpos", "Label size in the plot:",
                                min = 0, max = 50, value = 5))
    ),
    
    plotOutput("plot2"),
    tags$br(),
    
    
    fluidRow(
      column(6, downloadButton('downloadData4', 'Download Table', class = "btn-lg btn-success")), 
      column(6, downloadButton('downloadData3', 'Download Plot (Go/NoGo Prob)', class = "btn-warning"))
    ),
    
    tags$br(), 
    tags$br(),
    
    
    h3("Download Simulation Report"),
    downloadButton(outputId = "gng_report", class = "btn-danger", icon = icon("cloud-download-alt")),
    radioButtons(inputId = "report_format", label = "Choose a Format", choices = c("HTML", "PDF", "WORD"), selected = "HTML"),
    tags$br()
                           
  )
  
}

module_02$comp_01$server <- function(input, output, session, data){
  
  observe({
    if(rlang::is_empty(input$orrtx)){
        or1 <- NULL
      } else{
       or1 <- as.numeric(unlist(strsplit(input$orrtx, split = ",")))
      }
    updateSelectInput(inputId = "case", choices = or1, selected = or1[1])
  })
  
  
  
  
  psim <<- eventReactive(input$simulate,{
    
    or1 <- as.numeric(unlist(strsplit(input$orrtx, split = ",")))
    if (min(or1) > 0 & max(or1) < 1) {
      predsim(orr = or1, nmin = input$nmax[1], nmax = input$nmax[2],
              nsim = input$nsim, pptab = pptab(), groupsize = input$groupsize)
    }})
  
  tsim <<- eventReactive(input$simulate,{
    or1 <- as.numeric(unlist(strsplit(input$orrtx, split = ",")))
    if (min(or1)>0 & max(or1)<1) {
      psim1 <- tabsim_new(orr = or1, nmin = input$nmax[1], nmax = input$nmax[2],
                          input$nsim, pptab(), input$groupsize)
    }
    
    psim1[, 1] <- format(round(psim1[, 1]*100, 1), nsmall = 1)
    psim1[, 2] <- format(round(psim1[, 2]*100, 1), nsmall = 1)
    psim1[, 3] <- format(round(psim1[, 3]*100, 1), nsmall = 1)
    psim1[, 4] <- format(round(psim1[, 4]*100, 1), nsmall = 1)
    psim1[, 5] <- format(round(psim1[, 5]*100, 1), nsmall = 1)
    psim1[, 7] <- format(round(psim1[, 7]*100, 1), nsmall = 1)
    psim1[, 9] <- format(round(psim1[, 9]*100, 1), nsmall = 1)
    psim1[, 11] <- format(round(psim1[, 11]*100, 1), nsmall = 1)
    
    psim1[, 6] <- round(psim1[, 6], 1)
    psim1[, 8] <- round(psim1[, 8], 1)
    psim1[, 10] <- round(psim1[, 10], 1)
    
    psim1 <- psim1 %>% mutate_if(is.numeric, as.character) %>%
      mutate(case = paste0(case, "%"),
             efficacy = paste0(efficacy, "%"),
             es_confirm = paste0("(", es_confirm, "%)"),
             futility = paste0(futility, "%"),
             ef_confirm = paste0("(", ef_confirm, "%)"),
             early_decision = paste0(early_decision, "%"),
             es_prior_to_final = paste0(es_prior_to_final, "%"),
             ef_prior_to_final = paste0(ef_prior_to_final, "%")) %>%
      mutate(ef_confirm = ifelse(ef_confirm == "(NaN%)", ".", ef_confirm),
             es_confirm = ifelse(es_confirm == "(NaN%)", ".", es_confirm))
    
    
    colnames(psim1) <- c(paste("True ORR"),
                         "Efficacy", "(% confirmed with the final)",
                         "Futility", "(% confirmed with the final)", "Expected N",
                         "% Decision Prior to Final", "Expected N at Decision",
                         "% Decision Prior to Final", "Expected N",
                         "% Decision Prior to Final")
    
    psim1
  })
  
  
  
  library(knitr); library(kableExtra)
  output$table2 <- reactive({
    psim1= tsim()
    
    psim1 %>%  knitr::kable("html", align = rep("c", 11)) %>%
      kable_styling("striped", position = "center") %>%
      add_header_above(c(" " = 1, "Efficacy" = 2, "Futility" = 2, " " = 2,
                         "Efficacy Decision Only" = 2,
                         "Futility Stopping Only" = 2)) %>%
      column_spec(c(2, 3, 6:9), bold = TRUE, color = "purple") %>%
      column_spec(1:ncol(psim1), width = "5em") %>%
      
      add_footnote(label =
                     c(paste("Criteria: Target Value (TV) = ", input$px[2]*100, "%; ",
                             "Lower Reference Value (LRV) = ", input$px[1]*100,
                             "%; Go: Pr(ORR > LRV) > ", input$theta1*100,
                             "%; NoGo: Pr(ORR > TV) < ", input$theta2*100, "%;", sep = ""),
                       paste("Efficacy: PP(Go) > ", input$theta3*100,
                             "%); Futility: PP(NoGo) > ", input$theta4*100, "%); ",
                             "Frequency: ", input$groupsize,
                             " Subjects; BM ORR: Benchmark Objective Response Rate.", sep = "")
                     ), notation = "none")
  })
  
  
  
  output$downloadData3 <- downloadHandler(
    filename = "simplot.png",
    content = function(file) {
      res <- plotsim(psim(), input$px[1], input$px[2], as.numeric(input$case), input$lgdpos)
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
    #browser()
    plotsim(psim(), input$px[1], input$px[2], as.numeric(input$case), input$lgdpos)
  })


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