
modules <- new.env()
files <- dir(path = ".", pattern = "fun.R", all.files = TRUE, full.names = TRUE)
for(util_files in files){
  source(util_files)#, local = modules)
}

server <- function(input, output, data, session){
  
  # bslib::bs_themer()
  require("r4ct")
  require("ggplot2")
  require("magrittr")
  require("dplyr")
  
  
  # make all parameters reactive 
  # Decision table page ----------------------
  target1       <- reactive(input$target)
  e11           <- reactive(input$e1)
  e22           <- reactive(input$e2)
  nmax_perdose1 <- reactive(input$nmax_perdose)
  a1            <- reactive(input$a)
  b1            <- reactive(input$b)
  tox1          <- reactive(input$tox)
  dmethod1      <- reactive(input$dmethod)
  
  # Simulation page --------------------------
  nmax1         <- reactive(input$nmax)
  cohortsize1   <- reactive(input$cosize)
  nmax_perdose1 <- reactive(input$nmax_perdose)
  ptox1         <- reactive(input$truetox)
  dslv_start1   <- reactive(input$startdose)
  nsim1         <- reactive(input$nsim)
  simseed1      <- reactive(input$simseed)
  isim1         <- reactive({req(input$isim); input$isim})
  
  ## Estimating MTD page ---------------------
  neach1        <- reactive(input$neach)
  dlteach1      <- reactive(input$dlteach)
  target2       <- reactive(input$target2)
  yupper1       <- reactive(input$yupper)
  
  
  #  browser()

  # tabPanel = 1 --------------------------------------------------------------
  # ----------------------------------------------------------------------------------------------------------------------------
  dmat <- reactive({
    
    if(dmethod1() %in% c("mtpi", "mtpi2")){
      
      mtpi2_decision_matrix(cocap = nmax_perdose1(),
                            target = target1(),
                            a = a1(),
                            b = b1(),
                            tolerance1 = e11(),
                            tolerance2 = e22(),
                            tox = tox1(),
                            method = dmethod1())
  
    } else if (dmethod1() == "hybrid 3+3"){
      hybrid33_decision() 
    } else if (dmethod1() == "BOIN"){
      boin_decision(target_tox = target1(), cohort_cap = nmax_perdose1(), 
                    p.saf = input$saf, p.tox = input$uaf, cutoff.eli = input$tox)
    }
    
  })
  # validate a and b
  output$valid_prior <- renderText({
    if(a1() < 0 | b1() < 0){
      validate("Priors `a` and `b` must be POSITIVE!")
    }
  })
  
  
  observe({
    updateSliderInput(session, inputId = "saf", value = 0.6 * target1())
   updateSliderInput(session, inputId = "uaf", value = 1.4 * target1())
   ptox0 <- as.numeric(unlist(strsplit(ptox1(),",")))
   updateSliderInput(session, inputId = "startdose", max = length(ptox0))
    
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
  # ----------------------------------------------------------------------------------------------------------------------------
  
  dmat1 <- reactive({
    if(!rlang::is_empty(input$upload_dmat)){
      d1 <- readr::read_csv(input$upload_dmat$datapath, col_names = FALSE) %>% as.matrix()
      # browser()
    } else{
      d1 <- dmat() # use the decision table from the first page
    }
    return(d1)
  })
  
  
  output$upload_dmat <- renderPlot({
    #req(input$upload_dmat)
    plot_decision_matrix(dtab = dmat1())
  })
  
  
  # run_sim <- reactiveValues(objs = NULL)
  
  
  
  run_sim <- eventReactive(input$gosim, {
    
    ncpu <- parallel::detectCores()
    ptox0 <- as.numeric(unlist(strsplit(ptox1(),",")))
    
    # for 3+3 max N is 6 per dose
    if(dmethod1() == "hybrid 3+3"){
      nmax_perdose2 <- 6 
    } else{
      nmax_perdose2 <- nmax_perdose1()
    }
    
    run_parallel_sim(ncores = ncpu - 2, 
                     nsim = nsim1(), 
                     core_fun = run_dose_escalation, 
                     combine_method = bind_rows, 
                     seed = simseed1(), 
                     parallel = TRUE, 
                     file_to_source = files, 
                     package_used = NULL, 
                     verbose_show = FALSE, 
                     ptox = ptox0,  
                     nmax_perdose = nmax_perdose2, 
                     dslv_start = dslv_start1(), 
                     dmat = dmat1(), 
                     nmax = nmax1(), 
                     cohortsize = cohortsize1())
    
  })
  
  res0 <- eventReactive(input$gosim, {
    
    ptox0 <- as.numeric(unlist(strsplit(ptox1(),",")))
    # browser()
    process_multiple_sim(obj = run_sim(), ptox = ptox0, target = target1())
    # res0$oc <- tmp1$oc
    # res0$n_sum <- tmp1$n_sum 
    # res0$stop_reason <- tmp1$stop_reason
    })

  output$oc_table <- DT::renderDataTable({
    # res0()$oc
    oc0 <- res0()$oc
    names(oc0) <- c("Dose", "Tox Risk", "Prob. MTD Selected", "Prob. Dose Tested",
                    "Expected N (Tested)", "Expected DLT (Tested)", "Prob. DLT (Tested)",
                    "Expected Isotonic Estimate")

    oc0 %>% DT::datatable(rownames = FALSE) %>% DT::formatRound(digits = 3, columns = c(3:8))
  })

  output$n_table <- DT::renderDataTable({
    ntable0 <- res0()$n_sum
    names(ntable0) <- c("Type of Summary", "Min", "Median", "Max", "Mean", "SD")
    ntable0 %>% DT::datatable(rownames = FALSE) %>% DT::formatRound(digits = 3, columns = c(5, 6))
  })

  output$stop_table <- DT::renderDataTable({
    stop0 <- res0()$stop_reason
    names(stop0) <- c("Reason for Trial Stop", "Probability")
    stop0
  }, rownames = FALSE)

  
  # select example trial to view
  samp_trial <- reactive({
    run_sim() %>% dplyr::select(esca) %>% dplyr::slice(isim1()) %>% tidyr::unnest(1)
    })
  
  output$trial_path <- renderPlot({
   
    ptox0 <- as.numeric(unlist(strsplit(ptox1(),",")))
    plot_escalation_path(escalation_table = samp_trial(), ndose = length(ptox0), nmax1())
  })
  
  output$isim_result <- DT::renderDT({
    tmp0 <- samp_trial()
    names(tmp0) <- c("Step", "Cum. N", "N (Current Cohort)", "N Tox. (Current Cohort)", 
                     "Current Dose", "Current Decision", "Next Dose", "Skip The Dose?", 
                     "Skip Reason")
    DT::datatable(tmp0, rownames = FALSE)
    
  })

  output$isim_iso <- DT::renderDataTable({
    tmp <- samp_trial() %>% group_by(current_dose) %>%
      dplyr::summarise(n_current = sum(n_current), ntox = sum(ntox))

    tmp1 <- estimate_dlt_isoreg(cohort_size = tmp$n_current,
                        n_dlt = tmp$ntox, target = target1()) %>%
            mutate(dose = unique(tmp$current_dose)) %>%
            select(dose, everything())

    names(tmp1) <- c("Dose", "N", "DLTs", "Raw Estimate", "Isotonic Estimate", "MTD Indicator")
    DT::datatable(tmp1, rownames = FALSE) %>% DT::formatRound(digits = 3, columns = c(4, 5))

  })
  
  # tabPanel = 3 --------------------------------------------------------------
  # ----------------------------------------------------------------------------------------------------------------------------
  est_dlt <- eventReactive(input$goiso, {
    
    # a vector of cohort size
    n_cohort <- as.integer(unlist(strsplit(neach1(),","))) 
    # number of DLTs experienced in each cohort
    dlts <- as.integer(unlist(strsplit(dlteach1(),",")))
    
    if(length(n_cohort)!=length(dlts)) stop("Length of doses and that of DLTs do not match!")
    
    temp <- estimate_dlt_isoreg(cohort_size = n_cohort, n_dlt = dlts, target2())
    # temp$trueDLT <- trueDLT2
    names(temp) <- c("N for Each Dose", "# DLT", "Raw Est.", "ISO. Est.", "MTD") 
    temp$Dose <- 1:nrow(temp)
    temp <- temp %>% select(Dose, everything())
    temp
  })  
  
  
  output$iso <- DT::renderDataTable({
    est_dlt() %>% DT::datatable(rownames = FALSE) %>% DT::formatRound(digits = 3, columns = c(4, 5)) 
  })
  
  iso_fig <- reactive(plot_iso_estimate(dat1 = est_dlt(), target = target2(), yupper = yupper1()))
  
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