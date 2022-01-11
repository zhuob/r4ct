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
    ggplot2::labs(x = "Dose Level", y = "Toxicity Probability") + 
    ggplot2::theme(legend.position = "bottom", 
                   axis.text = ggplot2::element_text(size = 15),
                   legend.text = ggplot2::element_text(size = 12), 
                   axis.title = ggplot2::element_text(size = 18))  
}


#' Summary of simulation outcome
#'
#' @name _summary
#' @aliases nsubj_summary
#' @aliases mtd_oc_summary
#' 
#' @rdname _summary
#' @param subjs a matrix of subjects, cell [i, j] means number of subjects for
#'   jth dose in ith simulation
#' @param ntoxs a matrix of DLTs, cell [i, j] number of DLTs for jth dose in ith
#'   simulation
#' @param mtds a vector of index corresponding to MTD for each simulation
#' @param ... parameters can be passed through `mean()` or `sd()`
#'
#' @return a tibble
#' @export
#'
#' @examples
nsubj_summary <- function(subjs, ntoxs, mtds, ...){
  
  n_summary <- function(x, ...){
    tibble::tibble(Min = min(x, ...), Med = median(x, ...), Max = max(x, ...), 
                   Mean = mean(x, ...), SD = sd(x, ...))
  }
  
  # total N
  n_total <- rowSums(subjs) %>% n_summary(...)
  # total DLTs
  n_dlts  <- rowSums(ntoxs) %>% n_summary(...)
  # N at toxicity Doses
  n_at_tox_dose <- subjs[cbind(1:length(mtds), mtds)] %>% n_summary(...)
  # DLTs at Toxicity Doses
  dlt_at_tox_dose <- ntoxs[cbind(1:length(mtds), mtds)] %>% n_summary(...)
  
  r0 <- tibble::tibble(summary_type = c("Total N", "Total DLTs", "N at Toxic Dose", "# DLTs at Toxic Dose"), 
                       bind_rows(list(n_total, n_dlts, n_at_tox_dose, dlt_at_tox_dose)))
  
  return(r0)
  
}

#' @rdname _summary
mtd_oc_summary <- function(subjs, ntoxs, mtds, isoest){
  
  # percent that each dose was identified as MTD
  pct_mtd       <- table(mtds)/length(mtds)
  ndose         <- ncol(subjs)
  u1 <- setdiff(1:ndose, mtds)
  full_mtd <- c(pct_mtd, rep(0, length(u1)))
  names(full_mtd) <- c(names(pct_mtd), u1)
  full_mtd   <- full_mtd[sort(names(full_mtd))]
  # percent that each dose was tested
  dose_test_pct <- colMeans(subjs > 0)
  # number of subjects/DLTs in each dose level
  n_tested      <- apply(subjs, 2, mean)
  n_dlt         <- apply(ntoxs, 2, mean)
  # percent of DLT experienced for each dose level
  pct_dlt       <- colMeans(ntoxs > 0)
  isoest        <- colMeans(isoest, na.rm = TRUE)
  
  r0 <- tibble::tibble(pct_mtd = full_mtd, dose_test_pct = dose_test_pct, 
                       n_tested = n_tested, n_dlt = n_dlt, pct_dlt = pct_dlt, 
                       isoest = isoest)
  
  return(r0)
  
}


#' Simulation Summary
#'
#' @param obj the output from multiple simulation
#' @param ptox  a vector of probabilities of toxicity at each dose level
#' @param target  the target probability of toxicity
#' @return a list of two tables, one for OC and the other for sample size summary
#' @export
#'
#' @examples
process_multiple_sim <- function(obj, ptox, target){
  
  nsim <- nrow(obj)
  # transform results to table, so each row represents a simulation result
  ntoxs <- matrix(obj %>% select(ntox) %>%  tidyr::unnest(1) %>% 
                    as.matrix(), nrow = nsim, byrow = TRUE)
  subjs <- matrix(obj %>% select(nsubj) %>%  tidyr::unnest(1) %>%
                    as.matrix(), nrow = nsim, byrow = TRUE)
  
  find_mtd_index <- function(x, target){
    mtd1 <- estimate_dlt_isoreg(cohort_size = subjs[x, ], n_dlt = ntoxs[x, ], target = target)
    mtd_identified <- which(mtd1$mtd == "MTD")
    t0 <- tibble(iso = list(mtd1$est_iso), mtd_dose = mtd_identified)
    return(t0)
  }
  mtd0 <- purrr::map_dfr(.x = 1:nsim, .f = find_mtd_index, target = target)
  iso_est <- matrix(mtd0 %>% select(iso) %>% tidyr::unnest(1) %>% 
                    as.matrix(), nrow = nsim, byrow = TRUE)
  mtds <- mtd0$mtd_dose
  
  sum1 <- mtd_oc_summary(subjs = subjs, ntoxs = ntoxs, mtds = mtds, isoest = iso_est) %>% 
    mutate(dose = 1:length(ptox), ptox = ptox) %>% select(dose, ptox, everything())
  sum2 <- nsubj_summary(subjs = subjs, ntoxs = ntoxs, mtds = mtds)
  
  s0 <- table(obj$end_trial)/nsim
  sum3 <- tibble::tibble(end_reason = names(s0), prob = s0)
  
  return(list(oc = sum1, n_sum = sum2, stop_reason = sum3))
  
}



#' Plot dose escalation path for one simulation
#'
#' @param escalation_table The dose escalation path
#' @param ndose number of doses in total 
#' @param nmax number of subjects specified for the simulation
#'
#' @return a ggplot object
#' @export
#'
#' @examples
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
    scale_x_continuous(breaks = xlabels, limits = c(0, nmax + 2), 
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
