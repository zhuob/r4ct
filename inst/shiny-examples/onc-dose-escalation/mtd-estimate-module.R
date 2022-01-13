
#  Estimating MTD panel -------------------------------------------------------
mtd_estimation_module     <- function(){
    fluidRow(
    wellPanel(
      h3("Estimating MTD"),
      fluidRow(
        column(6, textInput(inputId = "neach", label = "N at each dose level (seperated by ',')", value = "3,6,3,6,3,6")), 
        column(6, textInput(inputId = "dlteach", label = "Number of DLTs at each dose level", value = "0,1,0,0,0,2")),
        column(12, sliderInput(inputId = "target2", label = "Choose probability of target toxcicity", min = 0, max = 1, step = 0.01, value = 0.30))
      )
    ), 
    
    actionButton("goiso", label = "Estimate MTD", icon = icon("arrow-circle-right"), class = "btn-success"),
    
    
    fluidRow(
      column(8, DT::dataTableOutput(outputId = "iso")
      ),
      column(4, conditionalPanel(condition = "input.goiso == true", 
                                 downloadButton(outputId = "dld_iso_tab", label = "Download Table", class = "btn-info"))
      )
    ),
    
    tags$br(),
    fluidRow(
      column(8, plotOutput("iso_est")),
      column(4, 
             conditionalPanel(condition = "input.goiso == true", 
                              sliderInput(inputId = "yupper", label = "Adjust scale on Y-axis", 
                                          min = 0.1, max = 1, value = 0.5, step = 0.01), 
                              sliderInput(inputId = "fig_width", label = "Figure Width (for download only)", 
                                          min = 1, max = 20, value = 7, step = 0.5), 
                              sliderInput(inputId = "fig_height", label = "Figure Height (for download only)", 
                                          min = 1, max = 20, value = 7, step = 0.5), 
                              downloadButton(outputId = "dld_iso_fig", label = "Download Plot", class = "btn-info")
             )
      )
    )
    
  )
}  
