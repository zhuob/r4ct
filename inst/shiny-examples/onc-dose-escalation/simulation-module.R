#   Simulation Panel -------------------------------------------------------
simulation_module <- function(){
  
    fluidRow(
    wellPanel(
      
      tags$strong(h3("Simulation Parameters")),
      fileInput(inputId = "upload_dmat", label = "Optional: Choose a CSV File to Replace Existing Decision Matrix", accept = c(".csv", ".xlsx", ".txt")),
      plotOutput(outputId = "upload_dmat"),
      
      fluidRow(
        column(6, 
               sliderInput(inputId = "nmax", label = h6("Maximum Sample Size for the Trial"), value = 40, min = 10, max = 100, step = 1)
        ),
        column(6, 
               sliderInput(inputId = "cosize", label = h6("Number of subjects at each cohort" , tags$strong("(note: each dose level can have multiple cohorts)")), value = 3,  min = 1,  max = 10,  step = 1)
        )
      ),  
      
      fluidRow(
        column(8, textInput(inputId = "truetox", label = "Enter a Vector of True DLT Probabilities (seperated by ',') ", 
                            value = "0.05,0.1,0.2,0.25,0.3,0.35")
        ),
        column(4, 
               sliderInput("startdose", label = "Choose dose level to start with", value = 3, min = 1, max = 6)
        )
      ),
      
      
      fluidRow(
        column(4, numericInput(inputId = "nsim", label = "Number of Simulations", value = 100, min = 1, max = 1e5)), 
        column(4, numericInput(inputId = "simseed", label = "Simulation Seed (for reproducibility purpose)", value = 1234))
      ),
      
      
    ), 
    actionButton(inputId = "gosim", label = "Run Simulation", icon = icon("forward"), class = "btn-success"),
    
    uiOutput(outputId = "simulation_output")

  )
    
}
