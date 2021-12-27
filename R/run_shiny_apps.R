

#' Run the shiny app with an example
#'
#' @description 
#' `r lifecycle::badge("experimental")` 
#' 
#' @param appname the example shiny app to be launched
#' @import shiny
#' @return a shiny app interface
#' @export  
#'
#' @examples
#'  
#'  # helpful instructions here https://deanattali.com/2015/04/21/r-package-shiny-app/
#' # launch_app(appname = "mTPI2")
launch_app <- function(appname){

  lifecycle::signal_stage("experimental", "launch_app()")
  
  # locate all the shiny examples that exist
  valid_examples <- list.files(system.file("shiny-examples", package = "r4ct"))
  
  valid_example_msg <- paste0("Valid Examples are: '", 
                              paste(valid_examples, collapse = "', '"), "'")
  
  # if an invalid example is given, throw an error 
  if(missing(appname) || !nzchar(appname) || !appname %in% valid_examples){
    stop("Please run `run_example()` with an valid example app as an argument.\n",
         valid_example_msg, call. = FALSE)
  }
  
  # find and launch the app
  app_dir <- system.file("shiny-examples", appname, package = "r4ct")
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Source all module files and store the code in a 'modules' environment object
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  file_to_source <- list.files(app_dir)
  # depending on the structure of shiny app, choose different ways to launch it
  if (sum(c("server.R", "ui.R") %in% file_to_source) == 2){
    # if there are separate server and ui part
    shiny::runApp(app_dir, display.mode = "normal")
    
  } else if("app.R" %in% file_to_source){
    # if the ui and server are both in "app.R"
    shiny::runApp(paste0(app_dir, "/", "app.R"), display.mode = "normal")
  } else{
    error_msg <- paste("The folder", app_dir, " should have least one of the following R files (Note: file name is case sensitive) for the app to run : \n 1.server.R and ui.R \n 2.app.R" )
    stop(error_msg)
  }
  
}
