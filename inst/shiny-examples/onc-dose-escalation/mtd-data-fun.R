#' given speficied parameter, simulate toxicity outcomes
#'
#' @param nsim number of simulations
#' @param ptox the toxicity levels
#' @param n0 for each cohort, the max number of subjects allowed (for simulation
#'   purpose, this number can be set large, e.g. 100)
#' @param seed simulation seed for reproducibility purpose
#'
#' @return a 3d array
#' @export
#'
#' @examples
#' ptox <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35);
#' nsim <- 1e4; n0 <- 100
#' x1 <- sim_toxmat(nsim, ptox, n0, seed = 1234)
#' rowMeans(apply(x1, c(1, 3), mean))
#' # to get a single simulation result; 
#' sim1 <- t(x1[, , 1])
sim_toxmat <- function(nsim, ptox, n0, seed = 1234){
  
  set.seed(seed)
  # number of doses
  ndose <- length(ptox)
  # Verify the matrix is created correctly such that the cell means are similar to scenario
  dat <- array(rbinom(n0 * ndose * nsim, 1, prob = ptox), c(ndose, n0, nsim))
  # apply(dat, c(1, 2), mean)
  # verify the probabilities are correct ------
  # rowMeans(apply(dat, c(1, 3), mean))
  # to get a single simulation result
  # sim1 <- t(dat[, , 1])
  return(dat)
  
}


# slice one simulatiion
get_one_sim_dat <- function(toxmat, cocap, isim){
  
  # in this way, each column represents a cohort 
  sim1 <- t(toxmat[, 1:cocap, isim])
  
  return(sim1)
  
}

# for a given cohort, calculate number of toxicity
get_tox_bycohort <- function(one_trial_dat, ncum_cohort, cohortsize, cocap, current_dose){
  
  nenrolled2 <- min(ncum_cohort + cohortsize, cocap) # if nsbj reaches max, cap it
  ntox_by_cohort <- sum(one_trial_dat[(ncum_cohort + 1):nenrolled2, current_dose])
  
  return(ntox_by_cohort)
  
}




# based on the current decision, decide where the next move is
next_move <- function(idecision){
  
  move_direction <- dplyr::case_when(idecision == "E" ~  1, 
                                     idecision %in% c("D", "DU") ~  - 1, 
                                     idecision == "S" ~ 0)
  
  return(move_direction)
  
}


# based on the current trial decide whici the next dose is
find_next_dose <- function(move_dir, current_dose, cocap, ncum_cohort, dose_decision){
  
    ndose <- length(ncum_cohort)
    
    next_dose <- move_dir + current_dose
    
    if(next_dose > ndose){
      
      next_dose <- NA; 
      skip_dose <- TRUE
      skip_reason <- "highest dose reached"
      
    } else if(next_dose < 0){
      
      next_dose <- NA; 
      skip_dose <- TRUE
      skip_reason <- "lowest dose reached"
      
    } else {
      
      # test whether the next dose level has reached DU 
      ind1 <- dose_decision[next_dose] %in% c("DU")
      # check if cohort cap is reached for the next dose level
      ind2 <- ncum_cohort[next_dose] >= cocap
      
      skip_reason <- dplyr::case_when(
        ind1 & !ind2 ~  "DU",
        ind2 & !ind1 ~  "cohort max n", 
        ind1 & ind2  ~  "D/DU & max n", 
        TRUE         ~  ""
      )
      
      skip_dose <- ifelse(ind1 + ind2, TRUE, FALSE)
      next_dose <- ifelse(ind1 + ind2, NA,   next_dose)
    }

    result0 <- tibble::tibble(next_dose, skip_dose, skip_reason)    
    
  return(result0)
  
}




run_dose_escalation_once <- function(one_trial_dat, dslv_start, dmat, nmax, cocap, cohortsize){
  
  # initiate parameters
  ndose            <- ncol(one_trial_dat) # number of doses
  escalation_table <- NULL
  step             <- 0  # number of decisions made
  current_dose     <- dslv_start
  ncum_cohort      <- rep(0, ndose)  # cumulative number of subjects in the current cohort 
  ncum_tox         <- rep(0, ndose)  # cumulative number of tox in the current cohort
  nenrolled        <- sum(ncum_cohort)  # number of subjects enrolled
  dose_decision    <- rep(NA, ndose) # the most recent decision made at each dose level
  
  while(nenrolled <= nmax){
    # how many decisions are made
    step <- step + 1 
    
    # enroll patients by cohort size; calculate number of toxicities seen in this cohort
    ntox <- get_tox_bycohort(one_trial_dat = one_trial_dat, 
                             ncum_cohort = ncum_cohort[current_dose], 
                             cohortsize = cohortsize, 
                             cocap = cocap, 
                             current_dose = current_dose)
    
    # update number of subjects & most updated decision in each cohort
    ncum_cohort[current_dose] <- ncum_cohort[current_dose] + cohortsize
    ncum_tox[current_dose]    <- ncum_tox[current_dose] + ntox
    # decision ; ntox = 0 corresponds to the first row of the decision matrix
    idecision <- dmat[ncum_tox[current_dose] + 1, ncum_cohort[current_dose]] 
    dose_decision[current_dose] <- idecision
    # where the next move level is
    move_dir  <- next_move(idecision)
    
    # number of subjects already enrolled
    nenrolled <- nenrolled + cohortsize; 
   
    # stop trial when 
    #  1. number of subjects reached cohort cap
    #  2. the next decision is DU
    #  3. total sample size is reached
    
    check_next_dose <- find_next_dose(move_dir, current_dose, cocap = cocap, ncum_cohort = ncum_cohort, dose_decision)
    
    # save the results
    res0 <- tibble::tibble(step = step, nenrolled = nenrolled, n_current = cohortsize, 
                           ntox = ntox, current_dose = current_dose,
                           current_decision = idecision)
    
    res0 <- dplyr::bind_cols(res0, check_next_dose)
    
    escalation_table <- dplyr::bind_rows(escalation_table, res0)
    
    if(is.na(res0$next_dose)){
      break
    }
    current_dose <- res0$next_dose
    
  }
  
  return(escalation_table)
  
} 


