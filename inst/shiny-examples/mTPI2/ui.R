# Define UI
ui <- fluidPage(
  ## add new theme 
  theme = bslib::bs_theme(),
  titlePanel("mTPI-2"),
  tabsetPanel(
    type="tab",
    tabPanel(
      "Dose Escalation Decision Table", 
      hr(),
      fluidRow(
        column(6,plotOutput("plot1"),offset=3),
        column(4, 
               sliderInput("target",
                           h6("Target Toxicity Rate"),
                           value = 0.3,
                           min = 0,
                           max = 1),
               sliderInput("tolerance1", 
                           h6("Equivalence Radius -"), 
                           value = 0.05, 
                           min = 0, 
                           max = 0.5,
                           step = 0.01),      
               sliderInput("tolerance2", 
                           h6("Equivalence Radius +"), 
                           value = 0.05, 
                           min = 0, 
                           max = 0.5,
                           step = 0.01)),   
        
        column(4,
               sliderInput("cocap",
                           h6("Cohort Cap"),
                           min = 0,
                           max = 50,
                           value = 9),
               sliderInput("tox",
                           h6("Unacceptable Toxicity: Prob(Overdosing)"),
                           min = 0,
                           max = 1,
                           value = 0.95, 
                           step = 0.01)),
        column(4,
               numericInput("a", "Beta prior: a:", 1, min = 0, max = 100),
               numericInput("b", "Beta prior: b", 1, min = 0, max = 100),
               downloadButton('downloadData1', 'Download Plot'),
               downloadButton('downloadData2', 'Download Full Table')
        ))), # end 1st tab
    tabPanel("Operating Characteristics", 
             sidebarPanel(
               fileInput("file1", "Optional: Choose a CSV File to Replace Existing Decision Matrix",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               numericInput("nsim", "Number of Simulations", min = 0, max = 100000, value = 1000),
               sliderInput("nmax",
                           h6("Maximum Sample Size for the Trial",
                              tags$strong(tags$span(style = "color:red", "(Must be a multiple of Group Size)"))),
                           54,
                           min = 10,
                           max = 100,
                           step = 1),
               sliderInput("ndose",
                           h6("Number of Doses"), 
                           6, 
                           min = 2, 
                           max = 10,
                           step = 1),
               sliderInput("cosize", 
                           h6("Group Size"), 
                           3, 
                           min = 1, 
                           max = 10,
                           step = 1), 
               # change made ---------------------------------
               uiOutput("secondSelection"),
               # end of change ---------------------------------
               
               textInput('trueDLT', 'Enter a Vector of True DLT Probabilities', "0.05,0.1,0.2,0.25,0.3,0.35"),
               actionButton("Gosimu",label="Simulate")),
             mainPanel(  tabsetPanel(type = "tabs",
                                     tabPanel("Operating Characteristics",DT::dataTableOutput("Simu1")),
                                     tabPanel("Summary Statistics",DT::dataTableOutput("Simu2"))))), # end 2nd tab
    
    tabPanel("Toxicity Estimation", 
             sidebarPanel(
               # fileInput("file2", "Load an Excel File(.xlsx) for Isotonic Regression Analysis",
               #           accept = c(
               #             "text/csv",
               #             "text/comma-separated-values,text/plain",
               #             ".csv",
               #             ".xlsx")
               # ),
               # hr(),
               textInput('Neach', 'N at each dose level', "3,6,3,6,3,6"),
               textInput('DLTeach', 'Number of DLTs at each dose level', "0,1,0,0,0,2"),
               #textInput('trueDLT2', 'Enter a Vector of True DLT Probabilities', "0.05,0.10,0.20,0.25,0.30,0.35"),
               sliderInput("target2",
                           h6("Target Toxicity Rate"),
                           value = 0.3,
                           min = 0,
                           max = 1),
               # hr(),
               # actionButton("dataloaded",label="Load Data"),
               hr(),
               actionButton("Goiso",label="Get Results")
               #downloadButton('downloadiso', 'Download Results')
             ),
             # sidebarPanel(tableOutput("showdata")),
             # mainPanel( DT::dataTableOutput("ISO"))) # end 3rd tab
             mainPanel(
               DT::dataTableOutput("ISO")),
             fluidRow(
               column(4),
               column(8, downloadButton('downloadData999', 'Download Plot'))),
             fluidRow(column(4),   
                      column(8, plotOutput("isoEst")))
    )
    
  ))
