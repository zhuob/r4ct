#' @title Run survival analysis
#' @details this function takes the snapshot and runs survival analysis to get
#'   the log rank test p-value, the 95% CI for hazard ratio, and the median
#'   survival time.
#'
#' @param snapshot the data set obtained from
#'   \code{\link[AmgOnc]{take_snapshot}}
#' @param pval_eff,pval_fu the significance level to claim a success/futility
#'   for interim analysis or success/failure for final: set \code{pval_eff =
#'   NA} and assign \code{pval_fu} a positive value between 0 and 1 if it's
#'   just for interim futility;  set \code{pval_fu = NA} and assign
#'   \code{pval_eff} a positive value between 0 and 1 if it's just for interim
#'   efficacy; if it's interim analysis for both efficacy and futility, then
#'   must have \code{pval_eff < pval_fu}; if it's for final analysis, then
#'   \code{pval_eff} and \code{pval_fu} must both be specified and set to be
#'   equal;
#' @param is_trt user-defined treatment group. If \code{is_trt = NA} then the
#'   second arm number shown in data will be the treatment arm.
#'
#' @return a data frame containing the results of the test
#' @export

survival_test <- function(snapshot, pval_eff = 0.025, pval_fu = NA,
                          is_trt = NA){

  arms <- sort(unique(snapshot$arm))
  if (is.na(is_trt)){
    # by default, arm = 2 shown in data is the treatment
    is_trt <- arms[2]
  }
  is_control <- arms[arms != is_trt]

  snapshot$arm <- factor(snapshot$arm, levels = c(is_control, is_trt))

  # log-rank test p-value
  surv <- survival::Surv(pfs, pfs_censor) ~ arm
  lr <- survival::survdiff(surv, data = snapshot)  # run log-rank test
  # chisq returns a two sided p value, so the p value should be halved
  # lr_pvalue <- pchisq(lr$chisq, length(lr$n)-1, lower.tail = FALSE) / 2
  z_lrt <- (lr$obs[1] - lr$exp[1]) / sqrt(lr$var[1,1])
  lr_pvalue <- pnorm(-z_lrt)

  # cox HR with 95% confidence interval
  cox <- summary(survival::coxph(surv,data=snapshot))
  hr <- cox$coefficients[,"exp(coef)"]
  hr_conf <- c(cox$conf.int[,"lower .95"], cox$conf.int[,"upper .95"])

  # median K-M survival
  KM <- survival::survfit(surv, type="kaplan-meier",
                          conf.type="log", data=snapshot)
  median_arms <- survminer::surv_median(KM)[,"median"]
  median_diff <- median_arms[2] - median_arms[1]

  # trial decision
  if ( is.na(pval_fu)){
    #  message("Runing interim analysis for efficacy only")
    if (lr_pvalue < pval_eff & hr < 1) {
      decision <- 'interim/success'} else {
        decision <- 'interim/continue'
      }
  } else if (is.na(pval_eff)) {
    #  message("Running interim analysis for futility only")
    if (lr_pvalue > pval_fu){
      decision <- "interim/futility"
    } else { decision <- "interim/continue"}
  } else if (pval_eff < pval_fu){
    #  message("running interim analysis for both efficacy and futility")
    if (lr_pvalue < pval_eff & hr < 1){
      decision <- "interim/success"
    } else if (lr_pvalue >= pval_eff & lr_pvalue <= pval_fu){
      decision <- "interim/continue"
    } else if (lr_pvalue > pval_fu) {
      decision <- "interim/futility"
    }
  } else if (pval_eff == pval_fu){
    #  message("runing primary analysis")
    if (lr_pvalue < pval_eff & hr < 1) {
      decision <- 'primary/success'} else{ decision <- 'primary/failure'
      }
  }

  # subjects number
  n <- sum(KM$n)
  event_size <- sum(snapshot$pfs_censor == 1)
  if("timecut" %in% names(snapshot)){
    timecut <- unique(snapshot$timecut)
  } else{
    timecut <- snapshot %>% filter(pfs_censor == 1) %>%
      mutate(pfs_ca = pfs + timein) %>%
      arrange(pfs_ca) %>% slice(n()) %>% pull(pfs_ca)

  }

  # output
  return(tibble::tibble(n=n, pvalue=lr_pvalue, decision=decision,
                        event_size = event_size, hr=hr,
                        lower=hr_conf[1], upper=hr_conf[2],
                        median_ctr=median_arms[1], median_trt=median_arms[2],
                        median_diff=median_diff,
                        timecut = timecut))
}






#' @title data snapshot by desired event size
#' @description this function calculates the time cut for desired event size and
#'   then the censor indicator. It has been verified against EAST software.
#' @param dat the data frame containing, at least, the following variables
#'  \itemize{
#'  \item{\code{timein }}{patient arrival time}
#'  \item{\code{pfs    }}{progression or surivival time}
#'  \item{\code{lfu    }}{lost to follow up time or dropout time}
#'  }
#' @param n_event desired number of events for analysis
#'
#' @return the same data with extra columns \code{timecut} (the calander
#' time cut), \code{pfs_censor} (the censoring indicator, with 1 = event and
#' 0 = censor), \code{ongoing} (whether the status is still ongoing by timecut).
#' @export
#'
#' @examples
#'
snapshot_by_event <- function(dat, n_event){

  dat <- dat %>% mutate(pfs_ca = timein + pfs, lfu_ca = timein + lfu)

  # get the calendar cut time
  cut_time <- dat %>% filter(pfs < lfu) %>% arrange(pfs_ca) %>%
    slice(n_event) %>% pull(pfs_ca)

  # derive sensoring indicator

  #  pfs_ca, lfu_ca and timecut
  #  case 1. pfs_ca <= lfu_ca  <  timecut  1
  #  case 2. pfs_ca <= timecut <= lfu_ca   1
  #  case 3. lfu_ca <= pfs_ca <= timecut   0
  #  case 4. lfu_ca <= timecut <= pfs_ca   0
  #  case 5. timecut < pfs_ca < lfu_ca     0
  #  case 6. timecut < lfu_ca < pfs_ca     0

  # create censor indicator
  # dat_censor <- dat %>% filter(timein < cut_time) %>%
  #   mutate(timecut = cut_time,
  #          pfs_censor = case_when(
  #            pfs_ca <= lfu_ca  & lfu_ca <= cut_time ~ 1,
  #            pfs_ca <= cut_time & timecut <= lfu_ca ~ 1,
  #            lfu_ca <= pfs_ca & pfs_ca <= cut_time ~ 0,
  #            lfu_ca <= timecut & timecut <= pfs_ca ~ 0,
  #            cut_time < pfs_ca | cut_time < lfu_ca ~ 0)) %>%
  #   select(-pfs_ca, -lfu_ca)
  #
  # create actual survival time

  dat_censor <- dat %>% filter(timein < cut_time) %>%
    rename(pfs_raw = pfs) %>%
    mutate(mint = pmin(lfu_ca, cut_time), timecut = cut_time,
           pfs = ifelse(pfs_ca > mint, mint-timein, pfs_raw),
           pfs_censor = ifelse(pfs_ca <= mint, 1, 0),
           ongoing = pfs_raw > cut_time & pfs_raw < lfu) %>%
    select(-pfs_ca, -lfu_ca, -pfs_raw, -mint)


  return(dat_censor)

}

