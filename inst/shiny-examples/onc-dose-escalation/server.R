
modules <- new.env()
for(util_files in dir(path = ".", pattern = "fun.R", all.files = TRUE, full.names = TRUE)){
  source(util_files)#, local = modules)
}


server <- function(input, output, data, session){
  
  require("r4ct")
  require("ggplot2")
  require("magrittr")
  
  # bslib::bs_themer()
  #  browser()

  # tabPanel = 1 --------------------------------------------------------------
  dmat <- reactive(mtpi2_decision_matrix(cocap = input$cocap,
                                target = input$target,
                                a = input$a,
                                b = input$b,
                                tolerance1 = input$e1,
                                tolerance2 = input$e2,
                                tox = input$tox,
                                method = input$dmethod))

  # validate a and b
  output$valid_prior <- renderText({
    if(input$a < 0 | input$b < 0){
      validate("Priors `a` and `b` must be POSITIVE!")
    }
  })
  
  dmat_plot <- reactive(plot_decision_matrix(dtab = dmat()))
  
  output$plot1 <- renderPlot({dmat_plot()})
  
  output$downloadData1 <- downloadHandler(
    filename = "dose-escalation-decision-plot.png", 
    
    content = function(file){
      
      out_dir <- setwd(tempdir())
      on.exit(setwd(out_dir), add = TRUE)
      
      out1 <- dmat_plot()
      ggsave(filename = file, plot = out1)
      
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = "dose-escalation-decision-table.csv", 
    
    content = function(file){
      
      out_dir <- setwd(tempdir())
      on.exit(setwd(out_dir), add = TRUE)
      # browser()
      out1 <- as.data.frame(cbind(1:nrow(dmat())-1, dmat()))
      names(out1) <- c("ndlt", paste("n=", 1:ncol(dmat())))
     
      readr::write_csv(out1, file, na = "")
      
    }
  )
  
  # tabPanel = 2 --------------------------------------------------------------
  
  # tabPanel = 3 --------------------------------------------------------------
  
  est_dlt <- eventReactive(input$goiso, {
    
    # a vector of cohort size
    n_cohort <- as.integer(unlist(strsplit(input$neach,","))) 
    # number of DLTs experienced in each cohort
    dlts <- as.integer(unlist(strsplit(input$dlteach,",")))
    
    if(length(n_cohort)!=length(dlts)) stop("Length of doses and that of DLTs do not match!")
    
    temp <- estimate_dlt_isoreg(cohort_size = n_cohort, n_dlt = dlts, input$target2)
    # temp$trueDLT <- trueDLT2
    names(temp) <- c("Dose", "N for Each Dose", "# DLT", "Raw Est.", "ISO. Est.", "MTD")
    temp
  })  
  
  
  output$iso <- DT::renderDataTable({
    est_dlt() %>% DT::datatable() %>% DT::formatRound(digits = 3, columns = c(4, 5)) 
  }, rownames = FALSE)
  
  iso_fig <- reactive(plot_iso_estimate(dat1 = est_dlt(), target = input$target2, yupper = input$yupper))
  
  output$iso_est <- renderPlot({
    iso_fig()
  }) 
  
  output$dld_iso_tab <- downloadHandler(
    filename = paste("isoreg-estimate.csv"), 
    content = function(file){
    
      tmp_dir <- setwd(tempdir())
      on.exit(setwd(tmp_dir))
      
      dat0 <- est_dlt()
      readr::write_csv(x = dat0, file = file)          
    }
  )
  
  output$dld_iso_fig <- downloadHandler(
    filename = paste("isoreg-estimate.png"), 
    content = function(file){
      tmp_dir <- setwd(tempdir())
      on.exit(setwd(tmp_dir))
      
      ggsave(filename = file, plot = iso_fig(), width = input$fig_width, height = input$fig_height)
    }
  )
  
  
}