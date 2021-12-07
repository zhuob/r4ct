module_04             <- new.env()
module_04$name        <- "Share Your Data"
module_04$id          <- "sharedata"
module_04$icon        <- "share-square"


# A good example is here

# https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_04$comp_01 <- new.env()

module_04$comp_01$ui <- function(){
  
  fluidPage(
    titlePanel("You are invited to contribute to our database!"),
    
    shinydashboard::box(title = h3("Figure Source"), width = 12,
        
        column(width = 12,
               textInput(inputId = "publication", label = "Publication Link\n")
        ),
        
        column(width = 12, 
               textInput(inputId = "figNo", 
                         label = "Figure Number in Publication \n(e.g., Figure 2 [B])", 
                         width = "500px")
        ),
        
        column(width = 6,
               textInput(inputId = "studyID", label = "Study Name")
               ), 
        
        column(width = 6,
               textInput(inputId = "nctID", label = "NCT Number")
        )
    
      ),
  
    shinydashboard::box(width = 12,
                        title = h3("Study Info."),
        column(width = 6, 
               selectInput(inputId = "ta", label = "Therapeutic Area", 
                           choices = c("---", "Oncology", "Cardiovascular", "Pediatrics", 
                                       "Hematology", "Inflammation"), selected = "---")
        ),
        
        # column(width = 6, 
        #        selectInput(inputId = "disease", label = "Disease Type",
        #                    choices = c("---", "Melanoma", "NSCLC", "SCLC", 
        #                                "Ovarian Cancer", "Multiple Myeloma", 
        #                                "Leukemia", "Breast Cancer"), 
        #                    selected = "---")
        #        ),
        
        
        column(width = 6, 
               selectInput(inputId = "phase", label = "Phase", 
                           choices = c("---", paste("Phase", 1:3)),
                           selected = "---")
               ),
        
        column(width = 6, 
               selectInput(inputId = "endpoint", label = "Data Type", 
                           choices = c("---", "OS", "PFS", "Other"),
                           selected = "---")
               ), 
        
        column(width = 6, 
               textInput(inputId = "disease", 
                         label = "Disease Type", 
                         width = "500px")
        ) 
        
        
        ),  
    
    shinydashboard::box(width = 12, title = h3("Contributor Info."),
                
        column(width = 6,textInput(inputId = "userName", label = "Name")),
        column(width = 6,textInput(inputId = "userEmail", label = "Email"))
        ), 
      
    actionButton(inputId = "submit", label = "Submit"), 
    
    h3("Upload your data"),
    fileInput("contributeData", "Upload Reconstructed Data (CSV)", 
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
     
    actionButton(inputId = "add2database", label = "Contribute to Database"),
  
    box(width = 12)
    
  # h3("Existing Data"),
  # 
  # DT::dataTableOutput("existingData")
  )
  
}


module_04$comp_01$server <- function(input, output, session, data){
  
  
  df_kk <- reactive({
    
      validate(
        need(input$studyID != "", label = "Study Name cannot be empty", message = TRUE)
      )
    
    validate(
      need(input$nctID != "", label = "NCT Number cannot be empty", message = TRUE)
    )
      validate(
        need(input$publication != "", label = "Please provide publication link", message = TRUE)
      )
      
    
      result <- tibble::tibble(
        `NCT Number` = input$nctID,
        `Study Name` = input$studyID,
        `Therapeutic Area` = input$ta, 
        `Disease Type` = input$disease,
        `Study Phase` = input$phase,
        `Data Type` = input$endpoint,
        `Figure No. in Publication` = input$figNo,
        `Publication Link` = input$publication, 
        `Contributor` = input$userName,
        `Email` = input$userEmail
      )
      
      new_name <- paste(result[, 1:7], sep = "-" , collapse = "-") %>% 
        stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
        stringr::str_replace_all(pattern = " ", replacement = "_")
      
      result <- result %>% mutate(new_name = new_name)
      result
      #readr::write_csv(result, paste0("database/info-", new_name, ".csv"))
  })

  
    
  df_contribute <- reactive({
    if(is.null(input$contributeData)){
      return(NULL)
    } else{
      readr::read_csv(input$contributeData$datapath)
    }
  })
  
  observeEvent(input$submit, {
    
    if(!is.na(input$studyID)){
      
      df01 <- df_kk()
      new_name <- paste0("info-", df01$new_name, ".csv")
      df01 <- df01 %>% dplyr::select(-new_name)
      if(new_name %in% current_data$Key){
        stop("Data already in database")
      } else{
        aws.s3::s3write_using(df01, FUN = readr::write_csv, object = new_name, 
                              bucket = "amgen-cfda-dswb-projects-adhoc/designinnovation/prj_shinyapp")
      
        } 
    } else{
      return(NULL)
    }
  })
  
  # output$existingData <- DT::renderDataTable({
  #   
  #   readr::read_csv("databaseSummary.csv")
  # })
  
  observeEvent(input$add2database, {
  
    if(!is.null(df_contribute())){
      new_name <- paste0(df_kk()$new_name, ".csv")
      if(new_name %in% current_data$Key){
        stop("Data already in database")
      } else{
        
        aws.s3::s3write_using(df_contribute(), FUN = readr::write_csv, object = new_name, 
                            bucket = "amgen-cfda-dswb-projects-adhoc/designinnovation/prj_shinyapp")
      
      # readr::write_csv(df_contribute(), paste0("database/", new_name, ".csv"))  
      }
    }
    
  })
  
}


