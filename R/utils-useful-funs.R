#' For a given event size and randomization ratio, calculate the boundaries for 
#' claiming significance. This is especially useful for group sequential trial 
#' with time-to-event endpoint.
#'
#' @param hr_bound the HR boundary, significance is declared if observed HR <
#'   \code{hr_bound}
#' @param zval the boundary at z-value level 
#' @param alpha the alpha level (if p value < alpha, significant)
#' @param n_event number of events considered for the analysis 
#' @param rand_ratio randomization ratio in favor of treatment group
#'
#' @return a data frame about the relationships between the metrics
#' @export
#'
#' @examples
#' get_tte_boundary(hr_bound = NA, zval = NA, alpha = 0.003808063, 
#'                  n_event = 199.8, rand_ratio = 1)
#' # the results should match the following from rpact
#' # design1 <- rpact::getDesignGroupSequential(
#' # kMax = 2, alpha = 0.025, beta = 0.1, 
#' # informationRates = c(0.6, 1), typeOfDesign = "asOF")
#' # rpact::getSampleSizeSurvival(design = design1, thetaH0 = 1, 
#' #                       pi2 = 0.05, hazardRatio = 0.7 )
#' # NOT RUN
get_tte_boundary <- function(hr_bound, zval, alpha, n_event, rand_ratio){
  
  if(is.na(hr_bound) + is.na(zval) + is.na(alpha) != 2){
    stop("only one of 'hr_bound', 'zval' and 'alpha' should be specified")
  }
  
  # assuming the estimated log-hazard ratio follows N(0, E/4)
  c1 <- 1/(1 + rand_ratio)
  p1 <- c1 * (1 - c1)
  
  if (!is.na(hr_bound)){ # if HR is given 
    zval <- -log(hr_bound) * sqrt(n_event * p1)
    alpha <- stats::pnorm(zval, lower.tail = FALSE)
  } else if (!is.na(zval)){
    alpha <- stats::pnorm(zval, lower.tail = FALSE)
    hr_bound <- exp(-zval/sqrt(n_event * p1))
  } else if (!is.na(alpha)){
    zval <- stats::qnorm(alpha, lower.tail = FALSE)
    hr_bound <- exp(-zval/sqrt(n_event * p1))
  }
  
  return(data.frame(hr_bound = hr_bound, z_value = zval, alpha = alpha))
  
}



#' @title Run survival analysis
#' @param time time to event data
#' @param censor indicator for censor or event, 0 = censored, 1 = event
#' @param arm indicator for arm. Has to be at least two arms
#' @param control the name of the control arm
#' @param conf_level The confidence level to calculate for estimated hazard ratio
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
#' res1 <- run_survival(time, censor, arm, control = "arm_1", conf_level = 0.95)
#'
run_survival <- function(time, censor, arm, control = NA, conf_level){
  
  if(length(time) <= 1 | length(unique(arm)) == 1){
    if(length(time) <= 1){
      warning("Dataset is too small, survival analysis won't be performed by`run_cox`.")
    } else if ( length(unique(arm)) == 1){
      warning("There's only ONE treatment group, survival analysis won't be performed by`run_cox`.")
    }
    return(
      tibble::tibble(
        nsubj = length(time), 
        n_event = sum(censor),
        pvalue = NA,  
        hr = NA, 
        lower95 = NA, 
        upper95 = NA
      )
    )
  }
  
  else {
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
    hr_ci <- exp(
      cox_obj$coefficients[, "coef"] + c(-1, 1)* 
        stats::qnorm((1-conf_level)/2, lower.tail = FALSE) *
        cox_obj$coefficients[, 'se(coef)']
    )  
    
    hr_output <- tibble::tibble(
      hr_est = exp(hr_est[, "coef"]), 
      hr_lower_ci = hr_ci[1], 
      hr_upper_ci = hr_ci[2],
      pval_logrank = lr_pvalue
      )
  
    
    # median K-M survival
    km <- survival::survfit(surv_obj, type = "kaplan-meier", conf.type = "log", data = data)
    median_arms <- summary(km)$table
    
    # median follow-up time
    follow_up <- survival::survfit(surv_rev, type = "kaplan-meier", conf.type = "log", data = data)
    median_fu_arms <- summary(follow_up)$table[, 'median']
    
    # mean follow-up time
    # tmp <- survival:::survmean(follow_up, rmean = 999)
    # mean_fu_arms<- tmp$matrix[, "rmean"]
    
    # remove the notes in building packages
    nsubj <- nevent <- p_logrank <- NULL
    
    med_result <- tibble::tibble(
      arm = arm_levels, 
      nsubj = km$n, 
      nevent = median_arms[, "events"],
      median_time = median_arms[, "median"],
      median_fu = median_fu_arms# , 
      # mean_fu = mean_fu_arms
    ) %>% tidyr::pivot_wider(
      names_from = "arm", values_from = c(nsubj, nevent, median_time, median_fu)
    )
    
    # consolidate results
    result <- dplyr::bind_cols(hr_output, med_result)
    
  }

  return(result)
  
}
