
## Dose Escalation table  ------------------------------------------------------
decision_table_module <- function(){
  fluidRow(
    
    fluidRow(
      column(12, selectInput(inputId = "dmethod", choices = c("mtpi", "mtpi2", "hybrid 3+3", "BOIN"), 
                  selected = "mtpi2", label = h3("Select a Dose Escalation Method"))
             )
      ), 
      
    # decision matrix parameters for mtpi/mtpi2
    conditionalPanel(condition = "input.dmethod == 'mtpi' || input.dmethod == 'mtpi2'", {
      wellPanel(h3("Tuning Parameters for mTPI/mTPI2 Decision Matrix"),
                
    
        fluidRow(
          column(12, sliderInput(inputId = "target", label = "Target Toxicity Rate", min = 0, max = 1, value = 0.3, step = 0.01)), 
          column(6, sliderInput(inputId = "e1",     label = "Equivalance Radius -", min = 0, max = 0.5, value = 0.05, step = 0.01)),
          column(6, sliderInput(inputId = "e2",     label = "Equivalance Radius +", min = 0, max = 0.5, value = 0.05, step = 0.01))
        ),
        
        fluidRow(
          column(8, 
                 fluidRow(
                   column(12, sliderInput(inputId = "nmax_perdose", label = "Max N per dose", min = 0, max = 50, value = 10, step = 1)), 
                   column(12, sliderInput(inputId = "tox",   label = "Unacceptable Toxicity: Prob(Overdosing)", min = 0, max = 1, value = 0.95, step = 0.01))
                   )
                 ), 
        
          column(2, 
                 offset = 1,
                 fluidRow(
                  column(12, numericInput(inputId = "a", label =  "Beta Prior (a)", value = 1)), 
                  column(12, numericInput(inputId = "b", label =  "Beta Prior (b)", value = 1)),
                  hr(),
                  textOutput(outputId = "valid_prior")
                  ) 
                )
          )
        )
      }),
    
    
    # Decision matrix parameters for 3 + 3 --------------------------------------
    # not really needed
    
    # Decision matrix parameters for BOIN
    
    conditionalPanel(condition = "input.dmethod == 'BOIN'", {
      wellPanel(
        
        fluidRow(
          column(12, sliderInput(inputId = "target", label = "Target Toxicity Rate", min = 0, max = 1, value = 0.3, step = 0.01)), 
          column(8,  sliderInput(inputId = "nmax_perdose", label = "Max N per dose", min = 0, max = 50, value = 10, step = 1))
          ), 
        
        h6(tags$strong("Optional Parameters [Default values are used. see `BOIN::get.boundary()` for more details]")),
        fluidRow(
          column(6, sliderInput(inputId = "saf",     
                                label = "the highest toxicity probability that is deemed 
                          subtherapeutic (i.e., below the MTD) such that dose escalation
                          should be made. The default value is 0.6 * target.", 
                                min = 0, max = 1, value = 0.3, step = 0.01)),
          
          column(6, sliderInput(inputId = "uaf",     
                                label = "the lowest toxicity probability that is deemed
                          overly toxic such that deescalation is required. The default value is 1.4 * target.", 
                                min = 0, max = 1, value = 0.5, step = 0.01)), 
          column(12, sliderInput(inputId = "tox",   label = "Unacceptable Toxicity: Prob(Overdosing)", 
                                 min = 0, max = 1, value = 0.95, step = 0.01))
          
        )
      )
    }),
    
    
    fluidRow(
      column(4,
             fluidRow(
               tags$br(),
               column(12, h3("Download"), offset = 0),
              # fluidRow(
               column(6, 
                      downloadButton('downloadData1', p('Decision Plot'), class = "btn-info"),
                      ),
               column(6, 
                      downloadButton('downloadData2', p('Decision Table'), class = "btn-info"))
               )
             ), 
      column(7, plotOutput(outputId = "plot1"), offset = 1)
     )

  )
}
