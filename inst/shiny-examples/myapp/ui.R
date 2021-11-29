ui <- function(){
  shiny::div(
    class = "sap",
    shiny::tags$h3("Input a number:"),
    shiny::numericInput(inputId = "n0", label = "number of obs.", value = 100),
    shiny::tags$br(),
    shiny::plotOutput("histplot"),
    shiny::tags$br(),
    shiny::tags$h1("This is the end of it")
  )
}
