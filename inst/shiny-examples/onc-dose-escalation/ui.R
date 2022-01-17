modules <- new.env()
files <- dir(path = ".", pattern = "-module.R", all.files = TRUE, full.names = TRUE)
for(util_files in files){
  source(util_files)#, local = modules)
}


ui <- fluidPage(
  
  theme = bslib::bs_theme(),
  navlistPanel(widths = c(3, 9),
      tabPanel("Escalation Table", decision_table_module()), 
      tabPanel("Simulation and Operating Characteristics", simulation_module()), 
      tabPanel("Toxicity Estimate", mtd_estimation_module())
             )
  
)
