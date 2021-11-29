

#' Run the shiny app with an example
#'
#' @return a shiny app interface
#' @export
#'
#' @examples
run_example <- function(){

  app_dir <- system.file("shiny-examples", "myapp", package = "pkgShinyApp")

  if(app_dir == ""){
    stop("Could not find example directory, try reinstalling the package.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
