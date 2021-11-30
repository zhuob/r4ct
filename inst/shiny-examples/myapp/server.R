server <- function(input, output){
  require("shinyapps4clinicaltrial")
  output$histplot <- renderPlot({hello_histgram(input$n0)})
}
