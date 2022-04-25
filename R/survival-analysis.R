#' @title Run survival analysis
#' @param time time to event data
#' @param censor indicator for censor or event, 0 = censored, 1 = event
#' @param arm indicator for arm. Has to be at least two arms
#' @param control the name of the control arm
#' @import survival
#' @return a data frame containing results of sample size, p-value, decision,
#'   number of events, hazard ratio with 95% confidence interval,
#'   median survival, median follow-up, ect
#' @export
# # @seealso \code{\link[pkg]{take_snapshot_event}}
#'
#' @examples
#' rand_arm(nsbj = 1, ratio = c(1, 1))
#' time <- c(5.68, 0.34, 4.94, 1.49, 4.72, 1.32, 3.48, 3.42, 3.41, 2.93)
#' censor <- c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1)
#' arm <- c("arm_1", "arm_2", "arm_1", "arm_2", "arm_2", "arm_1", "arm_2", "arm_1", "arm_1", "arm_2")
#' control <- "arm_1"
#' res1 <- run_survival(time, censor, arm, control = "arm_1")
#'
run_survival <- function(time, censor, arm, control = NA){
  
  arm_levels <- unique(arm)
  
  if(!is.na(control)){ # if there's control arm specified, re-order treatment levels
    arm_levels <- c(arm_levels[arm_levels == control], arm_levels[arm_levels != control])
  }
  
  data <- tibble::tibble(time, censor)
  data$arm <- factor(arm, levels = arm_levels)
  data$lfu_censor <- 1 - data$censor
  
  # if(is.na(strata_factor)){
    surv_obj <- survival::Surv(time, censor) ~ arm
    # to calculate median follow-up time
    surv_rev <- survival::Surv(time, lfu_censor) ~ arm
    
  # } else{
  #   # make sure if strata is given, it has the same length as time/censor/arm
  #   if(length(strata_factor) != length(time)){
  #     stop("'strata_factor' should be a vector of the same length with 'time'! ")
  #   }
  #   surv_obj <- survival::Surv(time, censor) ~ arm + survival::strata(strata_factor)
  #   # to calculate median follow-up time
  #   surv_rev <- survival::Surv(time, lfu_censor) ~ arm + survival::strata(strata_factor)
  #   
  # }

  # perform log-rank test 
  lr <- survival::survdiff(surv_obj, data = data)
  # obtain 1-sided p-value
  lr_pvalue <- stats::pchisq(lr$chisq, length(lr$n) - 1, lower.tail = FALSE) / 2 
  
  # Cox HR with CI
  cox_obj <- summary(survival::coxph(surv_obj, data = data))
  hr_est <- cox_obj$coefficients
  # if not hr > 1, change the direction of p value
  if(any(hr_est[, "exp(coef)"] > 1)){ lr_pvalue <- 1 - lr_pvalue }
  hr_confint <- cox_obj$conf.int
  hr_output <- tibble::as_tibble(rbind(NA, cbind(hr_est, hr_confint[, 2:4, drop = FALSE])))
  
  # median K-M survival
  km <- survival::survfit(surv_obj, type = "kaplan-meier", conf.type = "log", data = data)
  median_arms <- summary(km)$table
  
  # median follow-up time
  follow_up <- survival::survfit(surv_rev, type = "kaplan-meier", conf.type = "log", data = data)
  median_fu_arms <- summary(follow_up)$table[, 'median']

  # mean follow-up time
  tmp <- survival:::survmean(follow_up, rmean = 999)
  mean_fu_arms<- tmp$matrix[, "rmean"]
  
  # remove the notes in building packages
  nsubj <- nevent <- p_logrank <- NULL
  

  med_result <- tibble::tibble(
    arm = arm_levels, 
    nsubj = km$n, 
    nevent = median_arms[, "events"],
    p_logrank = lr_pvalue,
    median_time = median_arms[, "median"],
    median_fu = median_fu_arms, 
    mean_fu = mean_fu_arms
  )
  
  # consolidate results
  result <- cbind(hr_output[, -c(1, 6)], med_result) 
  result <- dplyr::select(result, arm, nsubj, nevent, p_logrank, dplyr::everything())
  
  return(result)
  
}
