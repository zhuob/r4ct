#' given speficied parameter, simulate toxicity outcomes
#'
#' @param nsim number of simulations
#' @param ptox a vector of probabilities of toxicity at each dose level
#' @param nmax_perdose for each dose level, the max number of subjects allowed
#'   (for simulation purpose, this number can be set large, e.g. 100)
#' @param seed simulation seed for reproducibility purpose
#'
#' @return a 3d array
#' @export
#'
#' @examples
#' ptox <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35);
#' nsim <- 1e4; nmax_perdose <- 100
#' x1 <- sim_toxmat(nsim, ptox, nmax_perdose, seed = 1234)
#' rowMeans(apply(x1, c(1, 3), mean))
#' # to get a single simulation result; 
#' sim1 <- t(x1[, , 1])
sim_toxmat <- function(nsim, ptox, nmax_perdose, seed = 1234){
  
  set.seed(seed)
  # number of doses
  ndose <- length(ptox)
  # Verify the matrix is created correctly such that the cell means are similar to scenario
  dat <- array(rbinom(nmax_perdose * ndose * nsim, 1, prob = ptox), c(ndose, nmax_perdose, nsim))
  # apply(dat, c(1, 2), mean)
  # verify the probabilities are correct ------
  # rowMeans(apply(dat, c(1, 3), mean))
  # to get a single simulation result
  # sim1 <- t(dat[, , 1])
  return(dat)
  
}


# slice one simulatiion
get_one_sim_dat <- function(toxmat, nmax_perdose, isim){
  
  # in this way, each column represents a cohort 
  sim1 <- t(toxmat[, 1:nmax_perdose, isim])
  
  return(sim1)
  
}

# for a given cohort, calculate number of toxicity
get_tox_bycohort <- function(one_trial_dat, ncum_cohort, cohortsize, nmax_perdose, current_dose){
  
  nenrolled2 <- min(ncum_cohort + cohortsize, nmax_perdose) # if nsbj reaches max, cap it
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


#' based on the current trial decide whici the next dose is
#'
#' @param move_dir the escalation indicator `1 = E`, `0 = S` and `-1 = D`
#' @param current_dose the current dose level
#' @param nmax_perdose the cohort cap size
#' @param ncum_cohort a vector for number of subjects in each cohort
#' @param dose_decision a vector for most recent decision made at each dose level
#'
#' @return
#' @export
#'
#' @examples
find_next_dose <- function(move_dir, current_dose, nmax_perdose, ncum_cohort, dose_decision){
  
    ndose <- length(ncum_cohort)
    
    next_dose <- move_dir + current_dose
    
    if(next_dose > ndose){
      
      next_dose <- NA; 
      skip_dose <- TRUE
      skip_reason <- "highest dose reached"
      
    } else if(next_dose == 0){
      
      if(sum(ncum_cohort) <= 3){ # if total subjects <= 3 and next dose is 0, change to stay
        
        next_dose <- current_dose; 
        skip_dose <- TRUE
        skip_reason <- "n total <= 3, change D to S"
        
      } else{
        next_dose <- NA; 
        skip_dose <- TRUE
        skip_reason <- "lowest dose reached"
        
      }
      
    } else {
      
      # test whether the next dose level has reached DU 
      ind1 <- dose_decision[next_dose] %in% c("DU")
      # check if cohort cap is reached for the next dose level
      ind2 <- ncum_cohort[next_dose] >= nmax_perdose
      
      skip_reason <- dplyr::case_when(
        ind1 & !ind2 ~  "DU",
        ind2 & !ind1 ~  "max n reached for next dose level", 
        ind1 & ind2  ~  "DU & max n", 
        TRUE         ~  ""
      )
      
      skip_dose <- ifelse(ind1 + ind2, TRUE, FALSE)
      next_dose <- ifelse(ind1 + ind2, NA,   next_dose)
    }

    result0 <- tibble::tibble(next_dose, skip_dose, skip_reason)    
    
  return(result0)
  
}




#' Run one trial of dose escalation
#'
#' @param one_trial_dat the simulated outcome matrix. For cell [i, j], value 1
#'   means the jth subject in ith dose experienced toxicity, while 0 suggests no
#'   toxicity.
#' @param dslv_start select a dose level to start
#' @param dmat the decision matrix used to run the simulation
#' @param nmax max number of subjects
#' @param nmax_perdose cohort cap for each dose
#' @param cohortsize the number of subjects enrolled for each cohort
#'
#' @return a tibble 
#' @export
#'
#' @examples
run_once <- function(one_trial_dat, dslv_start, dmat, nmax, nmax_perdose, cohortsize){
  
  # initiate parameters
  ndose            <- ncol(one_trial_dat) # number of doses
  escalation_table <- NULL
  step             <- 0  # number of decisions made
  current_dose     <- dslv_start
  ncum_cohort      <- rep(0, ndose)  # cumulative number of subjects in the current cohort 
  ncum_tox         <- rep(0, ndose)  # cumulative number of tox in the current cohort
  nenrolled        <- sum(ncum_cohort)  # number of subjects enrolled
  dose_decision    <- rep(NA, ndose) # the most recent decision made at each dose level
  
  while(nenrolled < nmax){
    # how many decisions are made
    step <- step + 1 
    
    # enroll patients by cohort size; calculate number of toxicities seen in this cohort
    cohortsize <- min(cohortsize, nmax - nenrolled, nmax_perdose - ncum_cohort[current_dose])
    ntox <- get_tox_bycohort(one_trial_dat = one_trial_dat, 
                             ncum_cohort = ncum_cohort[current_dose], 
                             cohortsize = cohortsize, 
                             nmax_perdose = nmax_perdose, 
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
    
    check_next_dose <- find_next_dose(move_dir, current_dose, nmax_perdose = nmax_perdose, ncum_cohort = ncum_cohort, dose_decision)
    
    # save the results
    res0 <- tibble::tibble(step = step, nenrolled = nenrolled, n_current = cohortsize, 
                           ntox = ntox, current_dose = current_dose,
                           current_decision = idecision)
    
    res0 <- dplyr::bind_cols(res0, check_next_dose)
    
    escalation_table <- dplyr::bind_rows(escalation_table, res0)
    
    if(is.na(res0$next_dose)){ # no next dose is available
      break
    }
    current_dose <- res0$next_dose
    
  }
  
  return(tibble::tibble(nsubj = list(ncum_cohort), 
               ntox = list(ncum_tox), 
               esca = list(escalation_table),
               last_decision = list(dose_decision),
               end_trial = res0$skip_reason))
  
} 




run_dose_escalation <- function(ptox,  nmax_perdose, dslv_start, dmat, nmax, cohortsize, seed){
  
  # simulate the outcome matrix 
  # set nsim to 1 so that each time, only one trial outcome is simulated
  set.seed(seed)
  sim_mat <- sim_toxmat(nsim = 1, ptox = ptox, nmax_perdose = nmax_perdose, seed = seed) 
  one_trial_dat <- get_one_sim_dat(toxmat = sim_mat, nmax_perdose = nmax_perdose, isim = 1)
  tmp1 <- run_once(one_trial_dat, 
                   dslv_start = dslv_start, 
                   dmat = dmat, nmax = nmax, 
                   nmax_perdose = nmax_perdose, 
                   cohortsize = cohortsize)

  return(tmp1)
}

#' @author Bin Zhuo \email{bzhuo@amgen.com}
#' @title A wrapper to run parallel simulations
#'
#' @param ncores number of cores needed
#' @param nsim number of simulation to run
#' @param seed the general seed, will generate a sequence of \code{nsim} subseed
#'   for each simulation
#' @param core_fun the function to be run in parallel. This usually corresponds
#'   to the `one_trial` function that you want to repeat \code{nsim} times
#' @param combine_method how should the parallel results be combined, e.g.,
#'   `rbind` or `bind_rows` for row bind, or `cbind` or `bind_cols` for column
#'   bind
#' @param parallel logical `TRUE` or `FALSE`, if `FALSE` use for loop instead of
#'   parallel. Sometimes `parallel = FALSE` is useful for debugging the function.
#' @param package_used packages to be used in the `foreach` loop. Only needed if
#'   you are running in Windows platform, otherwise just keep the default `NULL`
#' @param file_to_source set to `NULL` by default; if running on Windows
#'   platform, need to specify which `.R` files to be sourced in `foreach` loop;
#' @param verbose_show logical flag `TRUE` or `FALSE` enabling verbose messages.
#'   This can be very useful for trouble shooting.
#' @param ... arguments inherited from `core_fun`
#'
#' @return a tibble of requested form
#' @export
#'
#' @examples
#' run_one <- function(seed, x){
#' set.seed(seed)
#' mean(rnorm(1000, mean = x))
#' }
#'
#' run_two <- function(seed, x){
#'   set.seed(seed)
#'   tibble::tibble(y = mean(rnorm(1000, mean = x)))
#' }
#'
#' # if function returns a single value, can use `rbind` or `cbind`
#' temp1 <- run_parallel_sim(ncores = 2, nsim = 1000,
#'                           seed = 123, core_fun = run_one,
#'                           x = 10, parallel = TRUE,
#'                           combine_method = rbind)
#'
#'
#' # can swithc parallel to be FALSE to run for loop
#' temp2 <- run_parallel_sim(ncores = NA, nsim = 1000,
#'                           seed = 123, core_fun = run_one,
#'                           x = 10, parallel = FALSE,
#'                           combine_method = rbind)
#'
#' # if function returns a tibble, can use `bind_rows` or `bind_cols`
#' temp3 <- run_parallel_sim(ncores = 50, nsim = 1000,
#'                           seed = 123, core_fun = run_two,
#'                           x = 10, parallel = TRUE,
#'                           combine_method = bind_rows)

run_parallel_sim <- function(ncores, nsim, seed, core_fun,
                             combine_method = bind_rows, parallel = TRUE,
                             package_used = NULL, file_to_source = NULL,
                             verbose_show = FALSE, ...){
  
  # generate a sequence of seed
  set.seed(seed)
  subseed <- sample(1:1e7, nsim)
  
  if(!parallel){  # run for loop
    t1 <- NULL
    for(k in 1:nsim){ #print(k);
      t0 <- core_fun(seed = subseed[k], ...)
      t1 <- combine_method(t1, t0)
    }
    
  }  else if(parallel){ # go parallel
    
    ncores_avail <- parallel::detectCores()
    if(ncores >= ncores_avail){
      
      ncores <- floor(ncores_avail * 0.9)
      warning(paste("number of cores available is", ncores_avail,
                    ", using", ncores, "cores (90%) instead"))
    }
    
    # register cluster, windows and linux have different behaviors
    platform <- .Platform
    if(platform$OS.type == "windows"){
      # will need to source code and load packages within foreach loop
      cl <- parallel::makeCluster(ncores)
      
    } else { # when type = "FORK" used, no package will be asked to specify
      cl <- parallel::makeCluster(ncores, type = "FORK")
      
    }
    
    doParallel::registerDoParallel(cl)
    
    library(foreach)
    
    t1 <- foreach(k = 1:nsim, .combine = combine_method,
                  .packages = package_used, .verbose = verbose_show) %dopar% {
                    
                    if(platform$OS.type == "windows"){
                      
                      if(!is.null(file_to_source)){ # load the R files as needed
                        for(kk in 1:length(file_to_source)){
                          source(file_to_source[kk])
                        }
                      }
                    }
                    
                    # run the simulations
                    t0 <- core_fun(seed = subseed[k], ...)
                    return(t0)
                  }
    parallel::stopCluster(cl)
  }
  return(t1)
}



