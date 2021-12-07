module_02             <- new.env()
module_02$name        <- "Data Base"
module_02$id          <- "datahouse"
module_02$icon        <- "database"

# A good example is here

# https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()

## module UI
test_data_table_ui  <- function(id){
  ns <- NS(id)
  shiny::tagList(
    DT::dataTableOutput(outputId = ns("my_data_table"))
  )
}

## module server
test_data_table_server <- function(input, output, session ){
  ns = session$ns
  
  myValue <- reactiveValues(check = '')
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  ## separate the info files and data sets
  all_files <- current_data$data_avail
  info_index <- stringr::str_detect(all_files, "info-")
  ## read all the info data
  info_path <- sort(current_data$Key[info_index])
  data_path <- sort(current_data$Key[!info_index])
  
  index_tab <- reactive({
  
    # data3 <-aws.s3::s3read_using(FUN = readr::read_csv, object = info_path[1],
    #                              bucket = "amgen-cfda-dswb-projects-adhoc")
    
    read_s3 <- function(i){
      aws.s3::s3read_using(FUN = readr::read_csv, object = info_path[i],
                           bucket = "amgen-cfda-dswb-projects-adhoc")
    }
    
    index_table <- purrr::map_df(.x = 1:length(info_path), .f = read_s3)
      
    index_table %>% mutate(data_name = paste(`NCT Number`, `Study Name`, `Therapeutic Area`, 
                                             `Disease Type`, `Study Phase`, `Data Type`, 
                                             `Figure No. in Publication`, sep = "-") %>% 
                                       stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
                                       stringr::str_replace_all(pattern = " ", replacement = "_"))
  })
  
  
  my_data_table <- reactive({
 
    info_tab <- index_tab()
    
    dplyr::bind_cols(
      tibble::tibble(
        Actions = shinyInput(downloadButton, 
                             nrow(info_tab),
                             'button_',
                             ns = ns,
                             label = "Download",
                             onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))
                             )
        ),
     info_tab %>% select(-data_name)
    )
  })
  
  
  lapply(1:length(data_path), function(i){
    output[[paste0("button_",i)]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        
        dft <- aws.s3::s3read_using(FUN = readr::read_csv, object = data_path[i],
                                    bucket = "amgen-cfda-dswb-projects-adhoc")
        
        readr::write_csv(x = dft, path = file)
      }
    )
  })
  
  observeEvent(input$select_button, {
    print(input$select_button)
  })
  
  
  output$my_data_table <- DT::renderDataTable({
    DT::datatable(my_data_table(), escape = FALSE, 
              options = 
                list(
                  # columnDefs = list(list(targets = 1:6, width = '2px')),
                  scrollX = TRUE,
                  preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                )
    )
  })
}



##################  Module UI and server ######################################
module_02$comp_01 <- new.env()

module_02$comp_01$ui <- function(){
   fluidPage(
     h3("Existing Data"),
    test_data_table_ui(id = "test_dt_inside_module")
  )
    
}
module_02$comp_01$server <- function(data = NULL, input, output, session) {
  callModule(module = test_data_table_server , id = "test_dt_inside_module")
}



