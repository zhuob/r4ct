

#' Run the shiny app with an example
#' 
#' @param appname the example shiny app to be launched
#' @import shiny
#' @return a shiny app interface
#' @export  
#'
#' @examples
#'  # helpful instructions here https://deanattali.com/2015/04/21/r-package-shiny-app/
#' 
launch_app <- function(appname){

  
  # locate all the shiny examples that exist
  valid_examples <- list.files(system.file("shiny-examples", package = "shinyapps4clinicaltrial"))
  
  valid_example_msg <- paste0("Valid Examples are: '", 
                              paste(valid_examples, collapse = "', '"), "'")
  
  # if an invalid example is given, throw an error 
  if(missing(appname) || !nzchar(appname) || !appname %in% valid_examples){
    stop("Please run `run_example()` with an valid example app as an argument.\n",
         valid_example_msg, call. = FALSE)
  }
  
  # find and launch the app
  app_dir <- system.file("shiny-examples", appname, package = "shinyapps4clinicaltrial")
  
  shiny::runApp(app_dir, display.mode = "normal")
}
