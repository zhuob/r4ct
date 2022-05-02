
# calculate decision matrix 

divide_interval <- function(tolerance1, tolerance2, target, method = "mtpi2"){
  
  
  pt1 <- target - tolerance1 
  pt2 <- target + tolerance2	## target toxicity
  
  if(method == "mtpi"){
    pvi <- c(0, pt1, pt2, 1)
    
  } else if (method == "mtpi2"){
    pwdi <- tolerance1 + tolerance2					   ## START of modification for mTPI2
    li   <- c(seq(pt1, 0, by = -1 * pwdi), 0)  # the subintervals below EI (equivalence interval)
    hi   <- c(seq(pt2, 1, by = pwdi), 1)       # the subintervals above EI
    pvi  <- sort(unique(c(li, hi)))
    
  }
  
  pcat <- cut(pvi, breaks = c(-1, pt1, pt2, 1), labels = c("LI", "EI", "HI"))[-1]
  
  return(list(pvi = pvi, pcat = pcat))
  
}

#' @title Bayes Decision Table using modified Toxicity Probability interval (mTPI)
#' @details For each cohort size and number of DLTs observed, calculate the
#'   Bayes posterior probability, based on which a decision table is generated
#'   using mTPI2 (or mTPI) method.
#' @references{
#'   \insertRef{guo2017bayesian}{r4ct}
#' }
#'   
#' @importFrom Rdpack reprompt
#' @import magrittr
#'
#' @param cocap Cohort Cap, max number of subjects in a cohort
#' @param target Target Toxicity Rate
#' @param tolerance1 Equivalence Radius -(target tox - tolerance1 = lower acceptance value )
#' @param tolerance2 Equivalence Radius +(target tox - tolerance2 = upper acceptance value)
#' @param a parameter for beta distribution prior
#' @param b parameter for beta distribution prior
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#' @param method either `mtpi` using mTPI method, or `mtpi2` using mTPI2 method
#'
#' @return a decision matrix with rows being the number of toxicities, and
#'   column the corresponding number of subjects in that cohort
#' @export
#'
#' @examples
#' # create decision table in figure 3
#' x1 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi")
#' x2 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi2")
#' # create decision table in figure C1 of the mTPI2 paper
#' x3 <- mtpi2_decision_matrix(cocap = 10, target = 0.1, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.02, tox = 0.95, method = "mtpi")
#' x4 <- mtpi2_decision_matrix(cocap = 10, target = 0.1, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.02, tox = 0.95, method = "mtpi2")
#' 
mtpi2_decision_matrix <- function(cocap, target, a, b, tolerance1, tolerance2, tox, method = "mtpi"){
  
  get_interval <- divide_interval(tolerance1, tolerance2, target, method = method)
  pvi <- get_interval$pvi
  pcat <- get_interval$pcat # 2 means EI
  
  
  decision_info <- list()
  # number of toxicities observed
  decision_info$ntox  <- matrix(0:cocap, nrow = cocap + 1, ncol = cocap)
  # number of subjects 
  decision_info$nsubj <- matrix(1:cocap, nrow = cocap + 1, ncol = cocap, byrow = TRUE)
  # non-existing combos
  non_compatible <- decision_info$ntox > decision_info$nsubj 
  #decision_info$ntox[non_compatible]  <- NA
  decision_info$nsubj[non_compatible] <- 999
  
  # posterior parameters based on beta-binomial
  decision_info$posta <- a + decision_info$ntox
  decision_info$postb <- b + decision_info$nsubj - decision_info$ntox
  
  
  # unite probability mass calculation
  n_interval <- length(pvi) -1
  upm_by_interval <- array(NA, dim = c(n_interval, cocap + 1, cocap))
  pdel <- diff(pvi)
  
  for(i in 1:n_interval){
    
    prob1 <-  stats::pbeta(pvi[i],   decision_info$posta, decision_info$postb)
    prob2 <-  stats::pbeta(pvi[i+1], decision_info$posta, decision_info$postb)
    prob_diff <- prob2 - prob1
    upm_by_interval[i, , ] <- prob_diff / pdel[i]
    
  }
  
  # find the location of max upm for each cell
  max_upm_cell <- apply(upm_by_interval, c(2, 3), which.max)
  
  # assign D/E/S levels to each cell
  dtab <- matrix(NA, nrow = cocap + 1, ncol = cocap)
  target_tox_interval <-  which(pcat == "EI")
  dtab[max_upm_cell < target_tox_interval] <- "E"
  dtab[max_upm_cell > target_tox_interval] <- "D"
  dtab[max_upm_cell == target_tox_interval] <- "S"
  
  # decide unacceptable toxicity cells
  # Post.Prob(p > target | data) > tox; then DU
  untox_cell <- stats::pbeta(target, decision_info$posta, decision_info$postb, lower.tail = FALSE) > tox
  # Do not apply stoping rules until >= 3 patients have been evaluated at a dose
  untox_cell[, 1:2] <- FALSE
  dtab[untox_cell] <- "DU"
  dtab[non_compatible] <- NA
  
  row.names(dtab) <- paste("dlt=", 1:nrow(dtab)-1, sep = "")
  colnames(dtab) <- paste("n=", 1:ncol(dtab), sep = "")
  decision_info$decision_table <- dtab
  return(dtab)
  
}


#' Plot the decision matrix for DLT
#'
#' @param dtab a decision matrix with columns being the number of subjects, and
#'   rows representing corresponding DLTs.
#' @rdname mtpi2_decision_matrix
#' @return a ggplot object
#' @export
#' @examples
#' x1 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi")
#'    plot_decision_matrix(x1)
plot_decision_matrix <- function(dtab){
  
  ndlt <- decision <- NULL
  
  sub_dtab <- dtab
  ntox <- rep(0:(nrow(sub_dtab)-1), ncol(sub_dtab))
  nsubj <- rep(1:ncol(sub_dtab), each = nrow(sub_dtab))
  sub_dtab_reform <- tibble::tibble(ndlt = ntox, nsubj, decision = as.vector(sub_dtab)) %>%
   # dplyr::filter(!is.na(decision)) %>% 
    dplyr::mutate(nsubj = factor(nsubj, levels = unique(nsubj)), 
                  ndlt = factor(ndlt, levels = unique(ntox)), 
                  decision = ifelse(is.na(decision), "NA", decision)) %>% 
    dplyr::mutate(decision = factor(decision, levels = c("D", "DU", "E", "S", "NA")))
  
  ## generate the decision table
  fig <- ggplot2::ggplot(data = sub_dtab_reform, ggplot2::aes(x = nsubj, y = ndlt)) + 
    ggplot2::geom_tile(aes(fill = decision), color = "white", width = 0.5, height = 0.5) + 
    ggplot2::theme_bw() +
    ggplot2::scale_y_discrete(limits = rev(levels(sub_dtab_reform$ndlt))) + 
    ggplot2::scale_x_discrete(position = "top") + 
    ggplot2::scale_fill_manual(values = c( "D" = "#d34d2f", "DU" =  "#660000",  
                                           "E" = "#00FF33", "S" = "#CC9900", "NA" = "#CCCCCC"), 
                               labels = c("D", "DU", "E", "S", "NA")) + 
    ggplot2::theme(legend.position = "bottom", 
                   axis.text = ggplot2::element_text(face = "bold", size = 15), 
                   axis.title = ggplot2::element_text(face = "bold", size = 20), 
                   plot.title = ggplot2::element_text(face = "bold", size = 25)
    ) + 
    ggplot2::labs( x = "Number of Subjects at Current Dose", y = "Number of DLTs") 
  
  return(fig)
  
}




#' Given number of subjects and DLTs, estimate the MTD by isotonic regression
#' and empirical estimation
#'
#' @param cohort_size a vector of number of subjects in each cohort
#' @param n_dlt a vector of number of DLTs (must have the same length as
#'   \code{cohort_size})
#' @param target the target toxicity 
#' @param method The method used to perform isotonic regression estimate, either
#'   `selfCoded` for  self-implementation or `statsPackage` using
#'   \code{\link[stats]{isoreg}}. No matter what method you choose, the results
#'   should be identical.
#' @param du a vector with elements 1 or 0 recording whether DU is observed in
#'   the corresponding dose (1) or not (0)
#'
#' @return a table of results for the estimates
#' @export
#'
#' @examples
#'  n_dlt <- c(0,1,0,0,0,2); cohort_size <- c(3,6,3,6,3,6); du <- c(0, 0, 0, 0, 0, 0)
#' estimate_dlt_isoreg(cohort_size, n_dlt, du, target = 0.3)
#'  # it also allows cohort size of 0 in a given cohort
#'   n_dlt <- c(0,1,0,0,0, 0, 2); cohort_size <- c(3,6,3,0,6,3,6); du <- c(0, 0, 0, 0, 0, 0, 1)
#' estimate_dlt_isoreg(cohort_size, n_dlt, du, target = 0.3)
estimate_dlt_isoreg <- function(cohort_size, n_dlt, target, du, method = "selfCoded"){
  
  if(length(cohort_size) != length(n_dlt)){
    stop("The length of cohort does not match with that of n_dlt. Please check your input!")
  }
  
  y <- cohort_size; x <- n_dlt; target_tox <- target;
  use_y <- which(y != 0 & du != 1) # which cohort has enrolled patients and no DU
  # doses <- 1:length(y); 
  est_raw <- est_iso <- mtd <- rep(NA, length(y))
  
  if (method == "statsPackage"){
    tox_isoreg <- stats::isoreg(x[use_y]/y[use_y])$yf
  } else if(method == "selfCoded"){
    tox_isoreg <- isotonic_reg(x = x[use_y], y = y[use_y])
  }
  
  diff_target <- tox_isoreg - target_tox
  target_dose <- which(abs(diff_target) == min(abs(diff_target)))
  if(length(target_dose) == 1){
    result <- target_dose
  } else {
    diff1 <- diff_target[target_dose]
    if (min(diff1) >= 0 ){ # get the smallest if > target toxicity
      result <- utils::head(target_dose, 1)
    } else{ # if equidistance from left or right, pick the max dose from the left 
      result <- utils::tail(target_dose[which(diff1 < 0)], 1)
    }
  }
  final_result <- use_y[result] # the dose sequence for MTD  
  est_raw[use_y] <- x[use_y]/y[use_y]
  est_iso[use_y] <- tox_isoreg
  mtd[final_result] <- "MTD"
  
  indat <- tibble::tibble(# doses = doses, 
    n = cohort_size, dlts = n_dlt, du = du, 
    est_raw = est_raw, est_iso = est_iso, mtd = mtd)
  
  return(indat)
}


#' isotonic regression for estimating probability of MTD
#'
#' @param x vector of number of toxicities
#' @param y vector of number of subject at this dose level
#'
#' @return a vector of toxicity probabilities
#' @export
#' @keywords internal
# #' @examples
isotonic_reg <- function(x, y){
  
  if(length(x) != length(y)){
    stop("vector lengths do not match")
  }
  ndose <- length(x)
  p <- rep(NA, length(x))
  i <- 1
  while(i <= ndose){
    p1 <- cumsum(x[i:ndose])/cumsum(y[i:ndose])
    min_index <- which.min(p1) + sum(!is.na(p))
    p[i:min_index] <- min(p1)
    i <- min_index + 1
  }
  return(p)
}


#' BOIN Decision Matrix
#' @description Derive decision matrix with BOIN method
#' \loadmathjax
#' @details The decision matrix is derived by BOIN method. Given the target
#'   toxicity probability, it first calculates two quantities \mjseqn{\lambda_e}
#'   and \mjseqn{\lambda_d}, in which `De-escalation` decision is made if
#'   observed \mjseqn{\hat{p}\in (\lambda_d, 1)} and `Stay` decision is recommended when
#'   \mjseqn{\hat{p}\in (\lambda_e, \lambda_d]} and `Escalate` when
#'   \mjseqn{\hat{p} \in (0,\lambda_e]} according to 
#'   [FDA FFP-designated version of BOIN](https://www.fda.gov/media/155364/download).
#'    In the original BOIN paper, the boundary values are set as \mjseqn{(0, \lambda_e],
#'   (\lambda_e, \lambda_d)} and \mjseqn{[\lambda_d, 1)}.
#' @name boin_decision_matrix
#' @references{
#'   \insertRef{bayesyuany2016}{r4ct}
#' }
#' @rdname boin_decision_matrix
#' @param cocap cohort cap for each dose
#' @param target target toxicity probability
#' @param phi1 the highest toxicity probability deemed subtherapeutic such that
#'   dose-escalation should be made
#' @param phi2 lowest toxicity rate deemed overly toxic such that dose
#'   de-escalation is required
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#'
#' @export
#'
NULL 

#' @rdname boin_decision_matrix
#' @return for \code{boin_decision_matrix}, it returns a decision matrix with
#'   rows being the number of toxicities, and column the corresponding number of
#'   subjects in that cohort
#' @examples
#' # to repeat the supplementary table S1 in BOIN paper
#' target1 <- 0.25
#' x1 <- boin_decision_matrix(18, target1)

boin_decision_matrix <- function(cocap, target, phi1 = 0.6*target, phi2 = 1.4*target, 
                                 tox = 0.95){
  
  # calculate the optimal boundaries for dose-escalation rule, using the formula 
  # from the BOIN paper
  # if p < lambda_e then "E"; if lambda_e < p < lambda_d "S"; if p > lambda_d, "D"
  lambdas <- boin_tox_interval(target = target, phi1 = phi1, phi2 = phi2)
  lambda_d <- lambdas[2]; lambda_e <- lambdas[1]
  
  #  initialte decision matrix
  dtab <- matrix(NA, nrow = cocap + 1, ncol = cocap)
  row.names(dtab) <- paste("dlt=", 1:nrow(dtab)-1, sep = "")
  colnames(dtab) <- paste("n=", 1:ncol(dtab), sep = "")
  
  # initiate observed proportion of toxicity
  obs_tox <- dtab; 
  excessive_tox <- matrix(NA, nrow = cocap + 1, ncol = cocap)
  for(i in 1:cocap){
    ntox <- 0:i; nsubj <- i
    obs_tox[ntox+1, nsubj] <- ntox/nsubj 
    if(i >=3){ # if at least 3 subjects in the cohort, start DU assessment
      # note: this means the beta prior (1,1) is used to calculate probabilities
      excessive_tox[ntox+1, nsubj] <-stats::pbeta(target, ntox+1, nsubj-ntox+1, lower.tail = FALSE)
    }
  }
  # assign D/E/S levels to each cell
  dtab[obs_tox <= lambda_e] <- "E"
  dtab[obs_tox > lambda_e & obs_tox <= lambda_d] <- "S"
  dtab[obs_tox > lambda_d] <- "D"
  # unacceptable Toxicities
  dtab[excessive_tox > tox] <- "DU"
  
  return(dtab)
  
}
  


#' @rdname boin_decision_matrix
#' @return for \code{boin_tox_interval}, it returns a vector of length 2,
#'   corresponding to boundaries for escalation/de-escalation
#' @export
#' @examples 
#' # to reproduce the results in S3 of the supplementary table in the BOIN paper
#'  boin_tox_interval(target = 0.1, phi2 = 1.2*0.1)
boin_tox_interval <- function(target, phi1 = 0.6*target, phi2 = 1.4*target){
  
  # checking validity of parameters
  if (target < 0.05) {
    stop("the target is too low! ")
  }
  if (target > 0.6) {
    stop("the target is too high!")
  }
  # per the BOIN paper, setting phi1 or phi2 to close to the target makes it hard
  # to discriminate the target toxicity from rates close to it
  if ((target - phi1) < (0.1 * target)) {
    stop("the probability deemed safe cannot be higher than or too close to the target!")
  }
  if ((phi2 - target) < (0.1 * target)) {
    stop("the probability deemed toxic cannot be lower than or too close to the target!")
  }
  
  lambda_e <- log((1-phi1)/(1-target))/log((target*(1-phi1))/(phi1*(1-target)))
  lambda_d <- log((1-target)/(1-phi2))/log((phi2*(1-target))/(target*(1-phi2)))
  
  return(c(lambda_e, lambda_d))
  
}


