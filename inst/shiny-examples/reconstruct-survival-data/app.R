
# if (!file.exists("app.R")) {
#   stop("Current working directory must point to the dir containing app.R. Use setwd('/some/directory/')")
# }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Source all module files and store the code in a 'modules' environment object
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
require("shinydashboard")
require("shinyapps4clinicaltrial")
require("shiny")


modules <- new.env()
for (module_file in dir(".", pattern = "module_.*\\.R", full.names = TRUE)) {
  source(file = module_file, local = modules)
}


for (util_file in dir(".", pattern = "-fun.R", full.names = TRUE)) {
  source(file = util_file, local = modules)
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# data set available in the bucket
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# current_data <- shinyapps4clinicaltrial:::raw_data



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Build the UI
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- tagList(
  tags$head(
    tags$style(HTML("
        
                /* 
                Style the main dashboard page to only fill the main window 
                  and allow the tab body to scroll independently
                */
                body {
                        position: absolute !important;
                        top: 0px !important;
                        right: 0px !important;
                        bottom: 0px !important;
                        left: 0px !important;
                        overflow: hidden !important;
                }
                div.wrapper {
                        position: absolute !important;
                        top: 0px !important;
                        right: 0px !important;
                        bottom: 0px !important;
                        left: 0px !important;
                        overflow: hidden !important;
                }
                div.content-wrapper {
                        height: 100%;
                        overflow: hidden;
                }
                div.content-wrapper .content {
                        height: 100%;
                        overflow: auto;
                }
                        
        "))
  ),
  dashboardPage(
    dashboardHeader(title = "Reconstruct KM Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem(modules$module_01$name, tabName = modules$module_01$id, icon = icon(modules$module_01$icon)),
        menuItem(modules$module_03$name, tabName = modules$module_03$id, icon = icon(modules$module_03$icon))
       # menuItem(modules$module_04$name, tabName = modules$module_04$id, icon = icon(modules$module_04$icon)),
      #  menuItem(modules$module_02$name, tabName = modules$module_02$id, icon = icon(modules$module_02$icon))
      )
    ),
    dashboardBody(
      tabItems(
        
        # #+++++++++++++++++++++++++++++++++++++
        # Module 01
        #+++++++++++++++++++++++++++++++++++++
        tabItem(
          tabName = modules$module_01$id, verticalLayout(
            modules$module_01$comp_01$ui(),
            #modules$module_01$comp_04$ui(),
            modules$module_01$comp_02$ui()
          )
        ),
        
        #+++++++++++++++++++++++++++++++++++++
        # Module 02
        #+++++++++++++++++++++++++++++++++++++
        # tabItem(
        #   tabName = modules$module_02$id, verticalLayout(
        #     modules$module_02$comp_01$ui()
        #     #modules$module_02$comp_02$ui()#,
        #     #modules$module_02$comp_03$ui()
        #   )
        # )

        #+++++++++++++++++++++++++++++++++++++
        # Module 03
        #+++++++++++++++++++++++++++++++++++++
        tabItem(
          tabName = modules$module_03$id, verticalLayout(
            #modules$module_03$comp_01$ui(),
            modules$module_03$comp_01$ui()
          )
        )

        #+++++++++++++++++++++++++++++++++++++
        # Module 04
        #+++++++++++++++++++++++++++++++++++++
        # tabItem(
        #   tabName = modules$module_04$id, verticalLayout(
        #     #modules$module_03$comp_01$ui(),
        #     modules$module_04$comp_01$ui()
        #   )
        # )
        # 
        
        
        # ------------------------------------------------        
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #  Global Data Model 
  #   Note: we want to keep this 'data' object defined in the server function
  #         otherwise, this could become a globally shared variable across
  #         all users who are running on shiny server...we don't want that
  #         because we want each app to have its own unique session state.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data <- new.env()
  # data$histdata     <- rnorm(500)
  # data$cluster_data <- iris
  # 
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Execute all Module server() functions
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (module_name in names(modules) ) {
    module <- modules[[module_name]]
    for (element in names(module)) {
      component <- module[[element]]
      if (is.environment(component)) {
        component$server(input = input, output = output, session = session, data = data)
      }
    }
  }
  
}

shinyApp(ui, server)