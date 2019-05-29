
# Example in the paper
# Assumptions:
# 1. PD-L1 expression cutoff, >= 50%  or < 50%
# 2. Cor(ORR, PFS) > Cor(ORR, OS)
# 3. Phase 2 trial is to detect 0.55 HR with type 1 error 0.05
#    (one sided) and 80% power, with <= 0.67 as postive outcome to triggle P3
# 4. Event size = 70, sample size = 120 with 1:1 randomization
# 5. PFS = 10.3 months for treatment with PD-L1 >= 50%, 5-6 months for control
#    with PD-L1 < 50%.
# 6. Study duration: about 24 months for P2 with PD-L1 >=50%, and 16 months in
#    Population with PD-L1 < 50%
# 7. The P2 will expand seamlessly to P3 if a expansion decision is made.
# 8. In P3, an HR of 0.70 in OS at type 1 error 0.025 (one sided) and 90%
#    power. An emperical HR of 0.81 will prodice a postive outcome.
# 9. Adaptation analysis will be conducted at 90 patients with a minimum of 3
#    months follow-up. A p-value < 0.05 in ORR will trigger an expansion of
#    Phase 2 trial to P3.
# 10.A p-value < 0.025 for OS at the end of P3 (or earlier if GSD is used) in
#    case of expansion, or p-value < 0.025 for PFS at the end of P2 trial in
#    the case of no expansion is considered a positive result



#' @title Simulate data for all comers/enrichment population
#'
#' @param nsbj number of subjects to be simulated
#' @param alloc allocation vector, length corresponds to number of arms; as to
#'   be integer; enter 1 if single arm
#' @param b_size block size, has to be multiple of sum(alloc), enter 1 if single
#'   arm
#' @param rate enrollment rate per unit time
#' @param enrichment Is this data generated for enrichment population?
#' @param marker_prob vector of prevalence probability of different category, if
#'   doesn't add up to 1, will automatically standardize and generates warning.
#'   For enrichment population, just set \code{marker_prob = 1}
#' @param marker_name vector of names of different subgroup
#' @param par_trt_pos,par_trt_neg,par_ctrl_pos,par_ctrl_neg parameter
#'   specification for treatment/control and biomarker positive/negative
#'   population. For enrichment data generation, \code{par_trt_neg} and
#'   \code{par_ctrl_neg} are set to be \code{NULL}
#' @param ... other parameters from function \code{\link[AmgOnc]{enrl_dat_gen}}
#'
#' @return a tibble of simulated survival data
#'

cong_dat_gen <- function(nsbj, alloc = c(1, 1), b_size = 2, rate,
                         enrichment = FALSE,
                         marker_prob = c(0.7, 0.3),
                         marker_name = c("DLL3+", "DLL3-"),
                         par_ctrl_pos, par_ctrl_neg,
                         par_trt_pos, par_trt_neg, ...){

  if (enrichment){
    ## parameter specification for enrichment population
    pars <- list(list(par_ctrl_pos), list(par_trt_pos))

  } else{   ## parameter specification for all comers
    pars <- list(list(par_ctrl_pos, par_ctrl_neg),
                 list(par_trt_pos, par_trt_neg))
  }

  ## enrollment generation
  simu_enroll <- AmgOnc::enrl_gen(nsbj = nsbj, alloc = alloc,
                          b_size = b_size, rate = rate)

  ## data generation
  cong_dat <- AmgOnc::enrl_dat_gen(enrl = simu_enroll,
                                arg_list = pars,
                                marker_prob = marker_prob,
                                marker_name = marker_name, ...)
  return(cong_dat)
}




#' @title Trial process simulation
#' @description This function simulates survival data, performs enrichment
#'   analysis, based on which results, it enrolls biomarker positive population
#'   when enrichment is needed, or continues as the original trial when
#'   enrichment is not needed, and lastly, performs final analysis. Note that
#'   the final analysis is done on two analysis sets: all-comers and biomarker
#'   positive population; a win on either population will result in a positive
#'   outcome.
#' @rdname simu_trial
#' @name cong_simu_trial
#' @param n_allcomer number of subjects for all comers
#' @param n_enrichment increased sample size for enrichment group
#' @param alloc allocation vector, length corresponds to number of arms; as to
#'   be integer; enter 1 if single arm
#' @param b_size block size, has to be multiple of sum(alloc), enter 1 if single
#'   arm
#' @param rate enrollment rate per unit time
#' @param marker_positive,marker_negative a string specifying which marker is
#'   negative/positive
#' @param marker_prob vector of prevalence probability of different category, if
#'   doesn't add up to 1, will automatically standardize and generates warning
#' @param marker_name vector of names of different subgroup
#' @param sbj number of subjects for analysis of enrichment decision
#' @param ia_time_fu the follow-up time for decision of enrichment analysis
#' @param n_event the desired number of events for final analysis. Note that
#'   this parameter is used to decide time cutoff for final analysis; therefore
#'   \code{n_event} should be only counted among all-comer populations to proect
#'   the integraty of the trial.
#' @param cutoff the cutoff value to determine if enrichment is needed or not
#' @param alpha1,alpha2 significance level for testing all-comers or biomarker
#'   positive population, respectively
#' @param par_trt_pos,par_trt_neg,par_ctrl_pos,par_ctrl_neg parameter
#'   specification for treatment/control and biomarker positive/negative
#'   population
#' @return
#' @export
#'
#'
#'

cong_simu_trial <- function(n_allcomer, n_enrichment, alloc = c(1, 1),
  b_size = 2, rate, marker_positive = "DLL3+", marker_negative = "DLL3-",
  marker_prob = c(0.7, 0.3), sbj = 100, fu_time_ia = 2, n_event = 162,
  cutoff = 0, alpha1 = 0.0125, alpha2 = 0.0125,
  par_ctrl_pos = list(orr = 0.20, pfs_shape = 1, pfs_median = 7, corr = 0),
  par_ctrl_neg = list(orr = 0.10, pfs_shape = 1, pfs_median = 6, corr = 0),
  par_trt_pos = list(orr = 0.10, pfs_shape = 1, pfs_median = 6, corr = 0),
  par_trt_neg = list(orr = 0.30, pfs_shape = 1, pfs_median = 10, corr = 0)){

  marker_name <- c(marker_positive, marker_negative)

  simu_survival <- cong_dat_gen(nsbj = n_allcomer, alloc = alloc,
                               b_size = b_size, rate = rate,enrichment = FALSE,
                               marker_prob = marker_prob,
                               marker_name = marker_name,
                               par_trt_pos = par_trt_pos,
                               par_trt_neg = par_trt_neg,
                               par_ctrl_pos = par_ctrl_pos,
                               par_ctrl_neg = par_ctrl_neg)
  # the enrichment groups will be enrolled after allcomers are done enrolling
  timein_max <- max(simu_survival$timein)

  ia1 <- test_bm_neg(cong_dat = simu_survival,
                     marker_negative = marker_negative,
                     endpoint = "resp", sbj = sbj, fu_time_ia = fu_time_ia,
                     cutoff = cutoff)

  if (ia1$need_enrichment == "YES") {
    simu_enrichment <- cong_dat_gen(nsbj = n_enrichment, alloc = alloc,
                                    b_size = b_size, rate = rate,
                                    enrichment = TRUE, marker_prob = 1,
                                    marker_name = marker_positive,
                                    par_trt_pos = par_trt_pos,
                                    par_ctrl_pos = par_ctrl_pos,
                                    par_trt_neg = NULL, par_ctrl_neg = NULL) %>%
                       mutate(timein = timein + timein_max)

    ## combine the enriched population and all comers
    final_dat <- bind_rows(simu_survival,
                           simu_enrichment %>% mutate(id = n_allcomer + id))


  } else{
    final_dat <- simu_survival
  }

  ## find the calendar time for the desired number of events
  analysis_time <- final_dat %>% filter(id <= n_allcomer) %>%
  # id <= n_allcomer will ensure that n_events are counted within all-comers
                   mutate(event_time = timein + pfs) %>%
                   arrange(event_time) %>% slice(n_event) %>% pull(event_time)

  snapshot_dat <- AmgOnc::take_snapshot(final_dat, type = 1,
                sbj = NA, time = analysis_time) %>%
              mutate(population = if_else(id <= n_allcomer,
                                          "all comers", marker_positive))


  test_rslt <- cong_final_analysis(snapshot = snapshot_dat,
                           marker_positive = marker_positive,
                           alpha1 = alpha1, alpha2 = alpha2) %>%
               mutate(study_duration = analysis_time)

  rslt <- list(enrich_test = ia1, simu_rslt = test_rslt)

  return(rslt)
}


#' @rdname simu_trial
#' @name cong_simu_trial_parallel
#' @export
cong_simu_trial_parallel <- function(nsim = 10, ncores = 4, ...){

  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  use_function <- c("cong_dat_gen", "cong_final_analysis", "run_test",
                    "test_bm_neg", "cong_simu_trial")

  result <- foreach::foreach(i = 1:nsim, .combine = "rbind",
                             .export = use_function,
                             .packages = c("AmgOnc", "dplyr") ) %dopar% {

        temp <- cong_simu_trial(...)
        # whether the current trial need enrichment
        enrichment <- temp$enrich_test %>% pull(need_enrichment)
        rslt <- temp$simu_rslt %>%
          mutate(simu_id = i, enrichment = enrichment) %>%
          select(simu_id, enrichment, population, decision, everything())

      return(rslt)
    }

  doParallel::stopImplicitCluster()

  return(result)
}

