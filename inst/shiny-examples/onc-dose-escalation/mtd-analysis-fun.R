#' Plot isotonic regression estimate
#'
#' @param dat1 result from \code{estimate_dlt_isoreg}
#' @param target the target toxicity probability
#' @param yupper limit on the y-axis for better view
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plot_iso_estimate <-  function(dat1, target, yupper = 0.5){
  
  names(dat1) <- c("Doses","n","DLTs","Raw.Est","Iso.Est","MTD" )#, "trueDLT")
  
  dat_plot <- dat1 %>% dplyr::select(Doses, Raw.Est, Iso.Est ) %>% #, trueDLT) %>% 
    tidyr::pivot_longer(cols = 2:3, names_to = "Estimate", values_to = "pct") %>% 
    dplyr::mutate(Doses = factor(Doses), Estimate = ifelse(Estimate == "Raw.Est", "Emperical Estimate", "Isotonic Regression"))
  
  ggplot2::ggplot(data = dat_plot, ggplot2::aes(x = Doses, y = pct)) + 
    ggplot2::geom_point(ggplot2::aes(shape = Estimate, color = Estimate), size = 5) + 
    ggplot2::geom_hline(yintercept = target, color = "red", linetype = "dashed", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = seq(0, yupper, by = 0.05), 
                                limits = c(0, yupper), labels = function(x) paste0(x*100, "%")) +
    ggplot2::labs(x = "Doses", y = "Toxicity Probability") + 
    ggplot2::theme(legend.position = "bottom", 
                   axis.text = ggplot2::element_text(size = 15),
                   legend.text = ggplot2::element_text(size = 12), 
                   axis.title = ggplot2::element_text(size = 18))  
}



#' Run the simulation
#'
#' @param tdose true DLT probabilities
#' @param ndose total number of doses
#' @param nmax max number of subjects 
#' @param cosize the cohort size
#' @param target target toxicity level
#' @param tolerance1 Equivalence Radius -(target tox - tolerance1 = lower acceptance value )
#' @param nsim number of simulations to run
#' @param tolerance2 Equivalence Radius +(target tox + tolerance1 = lower acceptance value )
#' @param nmax_perdose max number of subjects for each cohort
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#' @param a,b prior parameters for beta distribution 
#' @param dslv_start Starting Dose to run simulation
#'
#' @return a list 
#' @export
#'
#' @examples
#' sim_mtpi2(tdose = c(0.05,0.1,0.2,0.25,0.3,0.35), ndose = 6,nmax = 30,
#' cosize = 3,target = 0.3, tolerance1 = 0.05,nsim = 1000, tolerance2= 0.05, 
#' nmax_perdose = 12,tox = 0.95,  a = 1, b = 1, dslv_start = 3)
sim_mtpi2 = function(tdose, ndose, nmax, cosize, target, tolerance1, nsim,  
                     tolerance2, nmax_perdose, tox,  a, b, dslv_start) {
  
  nds <- length(tdose)
  nds1 <- nds+1 
  nds2 <- nds+2
  if(ndose!=length(tdose)) stop("ERROR: Number of doses do not match input.")
  if(min(tdose)<=0) stop("ERROR: Dose input must be positive numbers")
  if(max(tdose)>=1) stop("ERROR: Dose input must be less than 1")
  
  mxn <- nmax 
  cohs0 <- cosize
  mxn1 <- mxn+1						## max N
  ncoh <- mxn/cohs0 
  ncoh1 <- ncoh+1						## no. of cohorts
  ncohd0 <- nmax_perdose	
  
  nmax_perdose <- 10; target <- 0.3; a = 1; b = 1; 
  tolerance1 = 0.05; tolerance2 <- 0.05; tox = 0.95
  decision_mat <- mtpi2_decision_matrix(nmax_perdose = nmax_perdose, 
                                        target = target, 
                                        a = a, 
                                        b = b, 
                                        tolerance1 = tolerance1, 
                                        tolerance2 = tolerance2, 
                                        tox = tox, 
                                        method = "mtpi2")
  Escf <- decision_mat
  
  outlist <- mTPIeval(B=nsim,scen=1,cohs=cohs0,ncohd=nmax_perdose,ncoh=ceiling(ncoh),
                      mxn=mxn,pt=pt,pt.a=pt1,pt.b=pt2,tdose=tdose,nds=nds,ncoh1=ceiling(ncoh1),
                      nds1=nds1,nds2=nds2,Escf=Escf,apr=apr,bpr=bpr,p.ud=p.ud, dslv_start = dslv_start)
  outlist
}



## BOIN Decision Matrix
boin_decision <- function(target_tox, cohort_cap, ...){
  
  bound <- BOIN::get.boundary(target = target_tox, ncohort = 100, 
                              cohortsize = 3)
  
  temp2 <- bound$full_boundary_tab %>% as_tibble() %>% 
    mutate(type = c("nsbj", "E", "D", "DU")) %>%
    gather(key = "nsbj", value = "DLT", -type) %>% 
    mutate(nsbj = as.numeric(str_extract(nsbj, "(\\d)+"))) %>%
    filter(type != "nsbj") %>% arrange(nsbj, DLT)
  temp2 <- temp2[complete.cases(temp2), ]
  
  nsbj0 <- min(max(temp2$nsbj), cohort_cap)
  temp3 <- purrr::map_df(1:nsbj0, .f = function(i) 
    bind_rows(tibble::tibble(nsbj = i, DLT = 0:i)))
  
  temp4 <- right_join(temp2 %>% arrange(nsbj, DLT), temp3, by = c("nsbj", "DLT"))
  temp5 <- temp2 %>% filter(nsbj <= nsbj0)%>% spread(key = type, value = DLT)
  temp6 <- left_join(temp5, temp3, by = "nsbj") %>% 
    mutate(decision = case_when(
      DLT <= E ~ "E",
      E < DLT & DLT < D ~ "S",
      DLT >= D & (DLT < DU | is.na(DU)) ~ "D", 
      DLT >= DU ~ "DU"
    ))
  
  temp6 <- temp6 %>% select(nsbj, DLT, decision) %>% 
    spread(key = nsbj, value = decision)
  
  return(temp6)
}


## 3 + 3 decision matrix
hybrid33_decision <- function(){
  
  decision <- tibble::tibble(DLT = seq(0, 6, by = 1), 
                             `3` = c("E", "S", "DU", "DU", rep(NA, 3)),
                             `6` = c("E", "E", rep("DU", 5)))
  return(decision)
}





plot_escalation_path <- function(escalation_table, ndose, nmax){
  
  decision_seq <- c("D", "DU", "E", "S")
  
  tmp1 <- escalation_table %>% dplyr::mutate(
    #nenrolled = factor(nenrolled, levels = sort(unique(escalation_table$nenrolled))),
    current_decision = factor(current_decision, levels = decision_seq))

  xlabels <-  unique(c(0, tmp1$nenrolled, nmax))
  tmp2 <- tmp1 %>% group_by(current_dose) %>% 
    mutate(n_current_cum = cumsum(n_current), ntox_cum = cumsum(ntox))
  
  show_text <- paste0(tmp2$ntox_cum, "/", tmp2$n_current_cum)
  
  fig <- ggplot(data = tmp1, aes(x = nenrolled, y = current_dose)) + 
    geom_point(aes(color = current_decision, shape = current_decision), size = 4) + 
    geom_line() + 
    scale_x_continuous(breaks = xlabels, limits = c(0, nmax), 
                       name = "Cummulative Number of Subjects Enrolled") + 
    scale_y_continuous(breaks = 0:ndose, limits = c(0, ndose), 
                       name = "Dose Cohort") + 
    guides(color = guide_legend(title = "Decision")) +
    guides(shape = guide_legend(title = "Decision")) + 
    scale_color_manual(values = c("#d34d2f", "#660000",  "#00FF33", "#CC9900"),
                      labels = decision_seq, drop = FALSE) +
    scale_shape_manual(values = c(15:18),
                       labels = decision_seq, drop = FALSE) + 
    geom_text(label = show_text, check_overlap = TRUE, nudge_y = -0.1, nudge_x = 0.5, color = "blue") +
    ggplot2::ggtitle("Dose Escalation Path", subtitle = "Note: for a given dose cohort, x/x = Cum. N toxicity/Cum. N enrolled")
    

  
    return(fig)  
  
}
