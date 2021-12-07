module_01             <- new.env()
module_01$name        <- "ReconstructData"
module_01$id          <- "shared_data"
module_01$icon        <- "toolbox"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_01$comp_01 <- new.env()

module_01$comp_01$ui <- function(){
    fluidPage(
      
      fluidRow(
        column(6, 
               fileInput("file1", "Upload Click Data (CSV)", 
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               ),
        
        column(6, 
               fileInput("file2", "Upload Risk Table (CSV)", 
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               
               )
      ),
    
      
      h3("Show Infile Data"), 
      tabsetPanel(
        tabPanel("Click Data", DT::dataTableOutput("click_data")), 
        tabPanel("Risk Table", DT::dataTableOutput("risk_table"))
      ),
      
      selectInput(inputId = "controlArm", label = "Identify Control Arm", 
                  choices = ""),
      hr() 
      
      )
}


module_01$comp_01$server <- function(input, output, session, data){
  
  ## use reactive so that the output can be passed as input for other modules
  df1_infile <- reactive({
    if(is.null(input$file1)){
      return(NULL)
    } else{
      readr::read_csv(input$file1$datapath)
    }
  })
  
  output$click_data <- DT::renderDataTable({
    df1_infile()
  })
  
  df2_infile <- reactive({
    if(is.null(input$file2)){
      return(NULL)
    } else{
      readr::read_csv(input$file2$datapath)
    }
  })
  
  output$risk_table <- DT::renderDataTable({
    df2_infile()
  })
  
  outVar <- reactive({
    if(is.null(input$file1)){
      return(NULL)
    } else{
      df1_infile() %>% select(arm) 
    }
    
  })
  
  observe({
    updateSelectInput(session, "controlArm", choices = outVar())
  })
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 02
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_01$comp_02 <- new.env()
module_01$comp_02$ui <- function(){
  
  shiny::fluidPage(
    h3("Convert KM-curve to Subject Level Data"),
   
    fluidRow(column(width = 6, align = "left", 
                    actionButton(inputId = "Calculate", label = "Reconstruct")),
             column(width = 6, align = "right", 
                    downloadButton(outputId = "DownloadData", label = "Download Data"))
             ),
    
    DT::dataTableOutput("recoverData"),
    
    h3("Add external data to compare?"),
    
    checkboxInput("addsurvData", "Yes"), 
    conditionalPanel(
      condition = "input.addsurvData == true", 
      fileInput("file3", "Upload External Data (CSV)", 
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    ),
    
    fluidRow(column(width = 6, align = "left", 
                    actionButton(inputId = "showPlot", label = "Run Analysis")),
              column(width = 6, align = "right", 
                     downloadButton(outputId = "DownloadFigure", label = "Download Figure"))
             ),
    
    
    # # figure size adjustment
    # fluidRow(column(width = 6, align = "left", 
    #                 sliderInput(inputId = "figh", label = "Fig height (in Pixel)", 
    #                             min = 100, max = 1000, value = 500))),
  

    tabsetPanel(
      tabPanel("Table Summary",   DT::dataTableOutput("showResult")),
      tabPanel("KM Plot", plotOutput("showKMcurve"))
  )
  
     
  
    
  )
}

#https://stackoverflow.com/questions/43038967/pass-renderui-input-from-one-shiny-module-to-another

module_01$comp_02$server <- function(input, output, session, data){
  
  
  v <- reactiveValues()
    
  observeEvent(input$Calculate, {
  
    req(input$file1);
    req(input$file2);
    
    df1 <- readr::read_csv(input$file1$datapath)
    df2 <- readr::read_csv(input$file2$datapath)
    
    v$clickdata <- df1
    v$risktable <- df2
    v$survdata <- reconstruct_survival_data(data_click = df1, data_nar = df2)
    
  })
  
  output$recoverData <- DT::renderDataTable({
    
    if(is.null(v$survdata)) return()
    v$survdata    
    
  })
  
  output$DownloadData <- downloadHandler(
    filename = "survival_data.csv", 
    content = function(file){
      readr::write_csv(v$survdata, file)
    }
  )
  
  
  v2 <- reactiveValues()
  observeEvent(input$showPlot, {
    
    if(input$addsurvData == TRUE){
      req(input$file3)
      df0 <- readr::read_csv(input$file3$datapath)
      
    } else {
      df0 <- NULL  
    }
    
    ## create the plot
    df_temp <- dplyr::bind_rows(v$survdata, df0)
    v2$fig <- show_reconstruct_km(recon_data = df_temp, data_nar = v$risktable)
    
    ## compute the key statistics for comparison
    arms <- unique(df_temp$arm)
    arm_control <- arms[arms == input$controlArm]
    arm_trt <- arms[arms != arm_control]
    
    result <- NULL
    for(i in 1:length(arm_trt)){
      df_analysis <- df_temp %>% filter(arm %in% c(arm_control, arm_trt[i]))
      result0 <- run_survival(time = df_analysis$time, censor = df_analysis$censor, 
                             arm  = df_analysis$arm,  control = arm_control) %>%
                 mutate(arm = arm_trt[i]) %>% select(arm, everything())
      
      result <- dplyr::bind_rows(result, result0)
    }
    
    v2$result <- result
    
  })
  
  output$showKMcurve <- renderPlot({
    if(is.null(v2$fig)) return()
    v2$fig
  }, height = 800, width = 800)
  
  
  output$showResult <- DT::renderDataTable({
    if(is.null(v2$result)){
      return()
    } 
    v2$result
  })
  # plotsize <- function(){
  #   size <- as.numeric(c(input$figureheight, input$figurewidth))
  #   return(list(height = size[1], width = size[2]))
  # }
  # 
  # output$showKMcurve <- renderUI({
  #   plotOutput("KMcurve", height = plotsize$height, width = plotsize$width)
  # })
  
  output$DownloadFigure <- downloadHandler(
    filename = "km.png",
    content = function(file){
      res <- v2$fig
      
      ggplot2::ggsave(filename = file, plot = res, width = 10, height = 8)
    }
  )
  
  
}

