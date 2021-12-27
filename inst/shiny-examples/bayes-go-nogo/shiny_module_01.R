module_01             <- new.env()
module_01$name        <- "Decision Table"
module_01$id          <- "shared_data"
module_01$icon        <- "toolbox"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_01$comp_01 <- new.env()

module_01$comp_01$ui <- function(){
  tabPanel("Decision Table",
                       fluidPage(
                        title="Decision Table",
                        
                        h3("Go/No-Go Plot"),                                  
                        plotOutput("plot1", width = "100%")
                        ),
           
                        hr(),
                        fluidRow(
                          column(3, 
                                 sliderInput("nmax",
                                             h6("First Interim (nmin) and Total (nmax)"),
                                             value = c(20, 40),
                                             min = 0,
                                             max = 100)
                                 ),
                          column(3,
                                 sliderInput("groupsize", 
                                             h6("# Enrolled after each Interim:"), 5,
                                             min = 0, max = 50)
                                 ), 
                          column(6, 
                                 sliderInput("px",
                                             h6("Lowest Reference Value and Target Value"),
                                             value = c(0.15, 0.3),
                                             min = 0,
                                             max = 1)
                                 )
                          
                          ),
                        
                        fluidRow(
                          column(6,
                                 sliderInput("theta1",
                                  h6("THETA1: Final Go Criteria: Prob(p > p0) > THETA1"),
                                     min = 0,
                                     max = 1,
                                     value = 0.8,
                                     step = 0.01)
                                 ),
                          column(6,
                                 sliderInput("theta2",
                                  h6("THETA2: Final NoGo Crit: Prob(p > p0) <= THETA2"),
                                     min = 0,
                                     max = 1,
                                     value = 0.1, 
                                     step = 0.01)
                                 )
                          ),
                        
                        fluidRow(
                          column(6, 
                                 sliderInput("theta3",
                              h6("THETA3: Interim Go Decision Threshold: Pred. P(Go) > THETA3"),
                                 min = 0,
                                 max = 1,
                                 value = 0.95,
                                 step = 0.01)
                              ),
                          column(6,     
                                 sliderInput("theta4",
                          h6("THETA4: Interim NoGo Decision Threshold: Pred. P(NoGo) > THETA4"),
                                 min = 0,
                                 max = 1,
                                 value = 0.95,
                                 step = 0.01)
                          )
                        ), 
                        
                        
                        h4("Priors"), 
                        fluidRow(column(4, numericInput("a", "Alpha:", 1, min = 0, max = 100)), 
                                 column(4, numericInput("b", "Beta:", 1, min = 0, max = 100))),
                        
                      fluidRow(
                        column(6, downloadButton('downloadData1', 'Download Plot', class = "btn-success")
                               ),
                        column(6,       
                               downloadButton('downloadData2', 'Download Full Table', class = "btn-success")
                               )
                      ), 
                      tags$br(),
                      tags$br()
                      )
  #)
}

module_01$comp_01$server <- function(input, output, session, data){

  pptab <<- reactive({
    nmax <- input$nmax[2]
    nmin <- input$nmax[1]
    p0 <- input$px[1]
    p1 <- input$px[2]
    bayes_pred_go_nogo(nmax = nmax, nmin = nmin, p0 = p0, p1 = p1, thetats = input$theta1,
          thetatf = input$theta2, thetau = input$theta3, thetal1 = input$theta4,
          a = input$a, b = input$b)
  })

  
  output$table1 <- renderTable(
    {pptab1=pptab()
    pptab2=cbind(rownames(pptab1), pptab1)}
  )


  output$plot1 <- renderPlot({
    gngplot(pptab(), input$groupsize)

  })


  output$downloadData1 <- downloadHandler(
    filename = "gngplot.png",
    content = function(file) {
      res <- gngplot(pptab(), input$groupsize)
      ggplot2::ggsave(filename = file, plot = res, width = 9, height = 8)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {paste('gngtable.csv')},
    content = function(file) {
      write.csv(pptab(), file)
    }
  )
  
}


