
#  Estimating MTD panel -------------------------------------------------------
mtd_estimation_module     <- function(){
    fluidRow(
    wellPanel(
      h3("Estimating MTD"),
      fluidRow(
        column(4, textInput(inputId = "neach", label = "N at each dose level (seperated by ',')", value = "3,6,3,6,3,6")), 
        column(4, textInput(inputId = "dlteach", label = "Number of DLTs at each dose level", value = "0,1,0,0,0,2")),
        column(4, textInput(inputId = "du", label = "whether DU is observed at each dose level (1 means 'Yes' and 0 means 'NO'", value = "0,0,0,0,0,1")),
        column(12, sliderInput(inputId = "target2", label = "Choose probability of target toxcicity", min = 0, max = 1, step = 0.01, value = 0.30))
      )
    ), 
    
    actionButton("goiso", label = "Estimate MTD", icon = icon("arrow-circle-right"), class = "btn-success"),
    # Uisng uiOutput because reactive and conditionalPanel does not function well simultaneously.
    uiOutput("mtd_estimate")

    
  )
}  
