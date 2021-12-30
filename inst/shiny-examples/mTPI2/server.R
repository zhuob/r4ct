modules <- new.env()
for (util_file in dir(".", pattern = "-fun.R", full.names = TRUE)) {
  source(file = util_file, local = modules)
}


# Define server logic 
server <- shinyServer(function(input, output, session) {
  
  require("r4ct")
  require("ggplot2")
  require("magrittr")
  
  output$secondSelection <- renderUI({
    sliderInput("startDose", 
                h6("Starting Dose"), 
                3, 
                min = 1, 
                max = input$ndose, 
                step = 1)
    
  })
  
  
  ## update the step in the slider bar
  
  observe({
    step_var <- input$cosize
    # num_dose <- ipnut$ndose
    # cohort_cap <- input$cocap
    updateSliderInput(session, inputId = "nmax", value = input$cocap*input$ndose, 
                      min = 2*step_var, max = 100, step = step_var)
  })
  
  # Code for Output  
  output$plot1 <- renderPlot({
    mtpi2_decision_plot(input$nmax, input$cocap,input$tolerance1,
                 input$tolerance2,input$a,input$b,input$tox,input$target)$decision_fig
  }
  )
  
  # download decision plot
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "go_nogo_plot.png"
    },
    content = function(file) {
      png(file,width=800, height=550)
      gonogo_plot <- mtpi2_decision_plot(input$nmax, input$cocap,input$tolerance1,
                   input$tolerance2,input$a,input$b,input$tox,input$target)
      print(gonogo_plot$decision_fig)
      dev.off()
    }
    
  )
  
  
  # Download the decision matrix as a CSV file (full matrix)
  output$downloadData2 <- downloadHandler(
    filename = function() {paste('gngtable.csv')},
    content = function(file) {
      xx =  mtpi2_decision_plot(input$nmax, input$cocap,input$tolerance1,
                         input$tolerance2,input$a,input$b,input$tox,input$target)$decision_table
      readr::write_csv(xx, file)
    }
  )
  
  observeEvent(input$file1, {
    values <- reactiveValues(df_data = NULL)
    values$df_data <- readr::read_csv(file=input$file1$datapath,header=T,check.names=F)
    xx <<- values$df_data
  })
  
  out <- eventReactive(input$Gosimu, {
    tdose <- as.numeric(unlist(strsplit(input$trueDLT,",")))
    sim_mtpi2(tdose,input$ndose,input$nmax,input$cosize,input$target,
          input$tolerance1,input$nsim,input$tolerance2,
          input$cocap,input$tox,input$a,input$b, input$startDose
    )
  })
  
  output$Simu1 <- DT::renderDataTable({ 
    cohs0 <- input$cosize
    mxn <- input$nmax
    ncohd0 <- input$cocap
    pt <- input$target
    pt1 <- pt-input$tolerance1
    pt2 <- pt+input$tolerance2	## target toxicity
    apr <- input$a
    bpr <- input$b					## hyper a b
    p.ud <- input$tox
    
    PmTPIDS <- out()$PerDS[,c(1,2,3,4,9,10,8,5,6,7)]
    PmTPIDS[,2]=round(PmTPIDS[,2]*100, 1)
    PmTPIDS[,3]=round(PmTPIDS[,3], 1)
    PmTPIDS[,4]=round(PmTPIDS[,4], 1)
    PmTPIDS[,5]=sprintf("%2.2f", PmTPIDS[,5])
    PmTPIDS[,6]=sprintf("%2.2f", PmTPIDS[,6])
    PmTPIDS[,7]=sprintf("%2.2f", PmTPIDS[,7])
    PmTPIDS[,8]=sprintf("%2.2f", PmTPIDS[,8])
    PmTPIDS[,9]=sprintf("%2.2f", PmTPIDS[,9])
    PmTPIDS[,10]=sprintf("%2.2f", PmTPIDS[,10])
    #print(PmTPIDS)
    
    colnames(PmTPIDS)=c("     ","       ")[c(2,2,2,2,2,1,1,1,2,1)]
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(PmTPIDS,
                  rownames = FALSE,
                  colnames = c("Doses Levels","Tox. Risk (%)","MTD Selected (%)","Dose Tested (%)",
                               "Expected N (Tested)","Expected DLTs (Tested)",
                               "DLT Rate (Tested)","Expected Isotonic Estimates",
                               "Expected N (Overall)","Expected DLTs (Overall)"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  )
                  
    )
  }) 
  
  output$Simu2 <- DT::renderDataTable({ 
    cohs0 <- input$cosize
    mxn <- input$nmax
    ncohd0 <- input$cocap
    pt <- input$target
    pt1 <- pt-input$tolerance1
    pt2 <- pt+input$tolerance2	## target toxicity
    apr <- input$a
    bpr <- input$b					## hyper a b
    p.ud <- input$tox
    
    tmp1 <- out()$PerTR
    tmp1[,1]=round(tmp1[,1], 0)
    tmp1[,2]=round(tmp1[,2], 1)
    tmp1[,3]=round(tmp1[,3], 1)
    tmp1[,4]=round(tmp1[,4], 0)
    tmp1[,5]=round(tmp1[,5], 2)
    
    
    tmp1[,5]=sprintf("%2.2f", tmp1[,5])
    
    
    colnames(tmp1)=c("      ","         ")[c(1,1,2,1,2)]
    rownames(tmp1)=c("Total N","Total DLTs","N at Toxic Doses","DLTs at Toxic Doses")
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(tmp1,
                  colnames = c("","Min.","Median","Mean","Max","Std.Dev"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  )
                  
    )
    
    
  })  
  
  res <- eventReactive(input$Goiso, {
    
    n <- as.integer(unlist(strsplit(input$Neach,",")))
    DLTs <- as.integer(unlist(strsplit(input$DLTeach,",")))
  
    if(length(n)!=length(DLTs)) stop("Number of doses and number of DLTs do not match!")
    
    temp <- estimate_dlt_isoreg(cohort_size = n, n_dlt = DLTs, input$target2)
    # temp$trueDLT <- trueDLT2
    
    temp
  })  
  
  prod_figure <- eventReactive(input$Goiso, {
    
    dat1 <- res()
    names(dat1) <- c("Doses","n","DLTs","Raw.Est","Iso.Est","MTD" )#, "trueDLT")
    
    dat_plot <- dat1 %>% dplyr::select(Doses, Raw.Est, Iso.Est ) %>% #, trueDLT) %>% 
      tidyr::pivot_longer(cols = 2:3, names_to = "Estimate", values_to = "pct") %>% 
      dplyr::mutate(Doses = factor(Doses), Estimate = ifelse(Estimate == "Raw.Est", "Emperical Estimate", "Isotonic Regression"))
    
    ggplot2::ggplot(data = dat_plot, ggplot2::aes(x = Doses, y = pct)) + 
      ggplot2::geom_point(ggplot2::aes(shape = Estimate, color = Estimate), size = 5) + 
      ggplot2::geom_hline(yintercept = input$target2, color = "red", linetype = "dashed", size = 1.5) + 
      ggplot2::scale_y_continuous(breaks = seq(0, 0.5, by = 0.05), 
                                  limits = c(0, 0.5), labels = function(x) paste0(x*100, "%")) +
      ggplot2::labs(x = "Doses", y = "Toxicity Probability") + 
      ggplot2::theme(legend.position = "bottom", 
                     axis.text = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 12), 
                     axis.title = ggplot2::element_text(size = 18))  
  }
  
  )

  output$ISO <- DT::renderDataTable({
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(res()[, -7],
                  colnames = c("Doses","n","DLTs","Raw.Est","Iso.Est","MTD"),# "trueDLT", "target"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  ))
  })
  
  output$downloadData999 <-  downloadHandler(
    filename = function() {
      "Est.png"
    },
    content = function(file){
      ggsave(file, prod_figure(), width = 10, height = 7)
    }
  ) 
  
  
  output$isoEst <- renderPlot({
    prod_figure()
  }
  
  )
  
})# end server
