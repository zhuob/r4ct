server <- function(input, output){
  output$histplot <- renderPlot({hello_histgram(input$n0)})
}
