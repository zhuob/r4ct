
## Dose Escalation table  ------------------------------------------------------
escalation_page <- fluidRow(
  
  tags$br(),
  wellPanel(h3("Tuning Parameters for Decision Matrix"),
    fluidRow(
      column(12, sliderInput(inputId = "target", label = "Target Toxicity Rate", min = 0, max = 1, value = 0.3, step = 0.01)), 
      column(6, sliderInput(inputId = "e1",     label = "Equivalance Radius -", min = 0, max = 0.5, value = 0.05, step = 0.01)),
      column(6, sliderInput(inputId = "e2",     label = "Equivalance Radius +", min = 0, max = 0.5, value = 0.05, step = 0.01))
    ),
    
    fluidRow(
      column(8, 
             fluidRow(
               sliderInput(inputId = "cocap", label = "Cohort Cap", min = 0, max = 50, value = 10, step = 1), 
               sliderInput(inputId = "tox",   label = "Unacceptable Toxicity: Prob(Overdosing)", min = 0, max = 1, value = 0.95, step = 0.01)
               )
             ), 
    
      column(2, 
             offset = 1,
             fluidRow(
              numericInput(inputId = "a", label =  "Beta Prior (a)", value = 1), 
              numericInput(inputId = "b", label =  "Beta Prior (b)", value = 1),
              hr(),
              textOutput(outputId = "valid_prior")
              ) 
            )
      )
    ),
  
  fluidRow(
    column(4,
           fluidRow(
             selectInput(inputId = "dmethod", choices = c("mtpi", "mtpi2"), selected = "mtpi2", label = "Select a Dose Escalation Method"), 
             tags$br(),
             column(6, h3("Download"), offset = 2),
            # fluidRow(
             column(5, 
                    downloadButton('downloadData1', p('Decision Plot'), class = "btn-info"),
                    ),
             column(5, 
                    downloadButton('downloadData2', p('Decision Table'), class = "btn-info"))
             )
           ), 
    column(6, plotOutput(outputId = "plot1"), offset = 0)
   )

)


#   Simulation Panel -------------------------------------------------------
simulation_page <- fluidRow(
  wellPanel(
  
  tags$strong(h3("Simulation Parameters")),
  fileInput(inputId = "upload_dmat", label = "Optional: Choose a CSV File to Replace Existing Decision Matrix", accept = c(".csv", ".xlsx", ".txt")),
  
  fluidRow(
    column(4, numericInput(inputId = "nsim", label = "Number of Simulations", value = 10000, min = 1, max = 1e7)), 
    column(4, numericInput(inputId = "simseed", label = "Simulation Seed (for reproducibility purpose)", value = 1234)), 
    column(4, textInput(inputId = "truedlt", label = "Enter a Vector of True DLT Probabilities (seperated by ',') ", 
                        value = "0.05,0.1,0.2,0.25,0.3,0.35"))
  ),
  
  fluidRow(
    column(6, 
           sliderInput(inputId = "nmax", 
                       label = h6("Maximum Sample Size for the Trial", tags$strong(tags$span(style = "color:red", "\n(Must be a multiple of Group Size)"))),
                       value = 54, min = 10, max = 100, step = 1), 
           sliderInput(inputId = "cosize", label = h6("Group Size"), value = 3,  min = 1,  max = 10,  step = 1)
           ),
    
    column(6, 
           sliderInput(inputId = "ndose", label = h6("Number of Doses"), value = 6, min = 2, max = 10, step = 1), 
           sliderInput("startdose", label = "Choose dose level to start with", value = 3, min = 1, max = 6)
          )
  )
  
), 
  actionButton(inputId = "gosim", label = "Run Simulation", icon = icon("forward"), class = "btn-success")
)


toxest_page     <- fluidRow(
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


ui <- fluidPage(
  
 # theme = bslib::bs_theme(),
  navlistPanel(widths = c(3, 9),
      tabPanel("Escalation Table", escalation_page), 
      tabPanel("Simulation and Operating Characteristics", simulation_page), 
      tabPanel("Toxicity Estimate", toxest_page)
             )
  
  
)
