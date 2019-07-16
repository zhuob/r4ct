#' @title Posterior probabilities for given sample size of n0 and n1
#' @description for a given pair of \code{n0} and \code{n1}, search all the
#'   possible sample space, calculates posterior probability, the probailities
#'   under the null and the alternative. This function is used to evaluate the
#'   power and type 1 error for a given pair of \code{n0} and \code{n1}
#' @details For a given pair of \code{n0} and \code{n1}, after specifying
#'   appropriate prior parameters, it calculates the posterior probability
#'   \eqn{P(p_1-p_0 >\delta)}, the probability \eqn{P(X_0 = x_0|n_0, p_0)\cdot
#'   P(X_1 = x_1|n_1, p_0)} under the null, and \eqn{P(x = x_0|n_0, p_0)\cdot
#'   P(X = x_1|n_1, p_1)}  under the alternative for each pair of of observed
#'   \eqn{x_0} and \eqn{x_1}. Note that \eqn{X_0,X_1} are assumed to be
#'   independent and follow binomial distribution.
#'
#'   The prior for control group, i.e. \eqn{p_0\sim Beta(a_0,b_0)},  are derived
#'   based on \eqn{a / (a + b) = p_0} and \eqn{a + b = n_0/2} where \eqn{n_0} is
#'   the sample size for control arm. The prior for treatment group is obtained
#'   such that \eqn{a_1 + b_1 = 2} and \eqn{a_1 = 2p_0}.
#'
#' @param n0,n1 the sample size for control/treatment group
#' @param p0 the underlying probability of response rate for the control arm
#' @param p1 the hypothesized ORR for treatment
#' @param delta the difference of the two proportions to be detected
#' @import dplyr
#' @return a tibble which contains the calculated probabilities
#'
#' @examples
#' library(dplyr)
#' r1 <- pos_two_grid(n0 = 75, n1 = 75, p0 = 0.59, p1= 0.812)
#' # power
#' r1 %>% filter(prob_post > 0.68) %>% select(prob_alt) %>% sum
#' # type 1 error
#' r1 %>% filter(prob_post > 0.68) %>% select(prob_null) %>% sum
#'
#' @seealso \code{\link[baseUtility]{pos_two}}
#' @export
#'
#'
pos_two_grid <- function(n0, n1, p0 = 0.25, p1 = 0.538,
                           delta = (p1-p0)/2, ab0 = NULL, ab1 = NULL){

    # calculate the prior parameters
    if(is.null(ab0)){
      ab0 <- prior_ab(n0, p0)
    }
    a0 <- as.numeric(ab0[1]); b0 <- as.numeric(ab0[2])
    # for the treatment, we set a + b = 2
    if(is.null(ab1)){
      ab1 <- prior_ab(4, p0) %>% as.vector()
    }
    a1 <- as.numeric(ab1[1]); b1 <- as.numeric(ab1[2])

    # expand the search grid
    n0_n1 <- expand.grid(x0 = 0:n0, x1 = 0:n1) %>%
             mutate(n0 = n0, n1 = n1, a0 = a0, b0 = b0,
                    a1 = a1, b1 = b1, delta = delta)

    # parameter prepared for posterior prob calculation
    params <- n0_n1 %>% dplyr::select(x0, n0, x1, n1, delta, a0, b0, a1, b1) %>%
                    as.list()

    prob_post <- purrr::pmap_dbl(.l = params, .f = baseUtility::pos_two)

    result <- n0_n1 %>% dplyr::mutate(prob_post = prob_post,
                               # probs under alternative
                               prob_alt = dbinom(x0, n0, p0)*
                                          dbinom(x1, n1, p1),
                               # probs under the null
                               prob_null = dbinom(x0, n0, p0)*
                                           dbinom(x1, n1, p0))
    return(result)

}


#' @title calculate prior parameters for a given beta distribution
#'
#' @param n the size of the prior beta distribution \eqn{a + b = n/2}
#' @param p the prior mean \eqn{\frac{a}{a + b} = p}
#'
#' @return the parameters \code{a} and \code{b} for \eqn{Beta(a,b)}
#' @export

prior_ab <- function(n, p){

  a <- n * p / 2
  b <- n * (1 - p) /2

  result <- tibble::tibble(a = a, b = b)
  return(result)

}


#' @title odds ratio and probabilities
#' @details  this two functions calculates odds ratio based on two
#'   probabilities, or probability of treatment given odds ratio and probability
#'   in the control arm
#' @param p0,p1 probability in the control/treatment arm
#' @param or  odds ratio which is expressed as \code{odds_treatment/odds_control}
#'
#' @return probability in the treatment arm, or odds ratio
#' @export
#' @examples
#'  p0 <- 0.59; p1 <- 0.812; or <- 3;
#'  calc_p1_or(p0 = p0, or = or);
#'  calc_p1_or(p0 = p0, p1 = p1)

calc_p1_or <- function(p0, p1 = NULL, or = NULL){

  if( is.null(or) & is.null(p1)) {
    stop("At least one of odds ratio or p1 should be non empty")
  }
  if (!is.null(or)){  # calculate p1
    return(p0 * or / (1 - p0 + p0 * or))
  } else if (!is.null(p1)) { # calculate or
    return( (p1 / (1 - p1)) / (p0 / (1 - p0)))
  }
}


#' @detail This function calculates a p value, either probability of success or
#'   failure
#' @title Calculate probability of success or failure
#' @param dat the object returned by \code{\link{pos_two_grid}}
#' @param cutoff the cutoff value to claim a success/failure
#' @param prob1 the posterior probability
#' @param prob2 the probability under the null or the alternative
#' @param eval_success Is this for evaluating probability of success?
#'
#' @return a p value
#' @export

tail_prob <- function(dat, cutoff, prob1, prob2, eval_success = TRUE){

  prob1 <- rlang::enquo(prob1)
  prob2 <- rlang::enquo(prob2)

  if(eval_success){ # if evaluating success: P(p1 - p0 > delta) > U
    temp <- dat %>% filter(!!prob1 > cutoff) %>% select(!!prob2)

   } else{  # if evaluating futility: P(p1 - p0 > delta) < L
    temp <- dat %>% filter(!!prob1 < cutoff) %>% select(!!prob2)
   }
  if(nrow(temp) == 0){pval <- 0}
  else {pval <- sum(temp[, 1])}

  return(pval)
}




#' @title Power and type 1 error calculation by grid search
#' @description for a given pair of \code{n0} and \code{n1}, search all the
#'   possible sample space, calculates posterior probability, the probailities
#'   under the null and the alternative. This function is used to evaluate the
#'   power and type 1 error for a given pair of \code{n0} and \code{n1}
#' @details For a given pair of \code{n0} and \code{n1}, after specifying
#'   appropriate prior parameters, it calculates the posterior probability
#'   \eqn{P(p_1-p_0 >\delta)}, the probability \eqn{P(X_0 = x_0|n_0, p_0)\cdot
#'   P(X_1 = x_1|n_1, p_0)} under the null, and \eqn{P(x = x_0|n_0, p_0)\cdot
#'   P(X = x_1|n_1, p_1)}  under the alternative for each pair of of observed
#'   \eqn{x_0} and \eqn{x_1}. Note that \eqn{X_0,X_1} are assumed to be
#'   independent and follow binomial distribution.
#'
#'   The prior for control group, i.e. \eqn{p_0\sim Beta(a_0,b_0)},  are derived
#'   based on \eqn{a / (a + b) = p_0} and \eqn{a + b = n_0/2} where \eqn{n_0} is
#'   the sample size for control arm. The prior for treatment group is obtained
#'   such that \eqn{a_1 + b_1 = 2} and \eqn{a_1 = 2p_0}.

#' @param p0 response rate in the control arm
#' @param p1 the hypothesized ORR for treatment
#' @param ... other parameters inherited from \code{\link{pos_two_grid}}
#' @param n0 sample size for control, can be a vector
#' @param n1 sample size for treatment, must be of the same length as \code{n0}
#' @param cutoff the cutoff value (can be a vector) to claim a decision (either
#'   success or failure)
#' @param eval_success Is this for evaluating probability of success? If
#'   \code{TRUE}, then it evaluates \deqn{P(p_1-p_0 > \delta) > U;} otherwise
#'   it evaluates \deqn{P(p_1-p_0>\delta)<L,} where \eqn{L} or \eqn{U} correspond
#'   to \code{cutoff}.
#' @param ncores number of cores to be used for fast parallel computing. If not
#'   specified, it will use  number of cores available - 1
#' @param ab0 a data frame or NULL. If a data frame, it should contain, in each
#'   row, the prior for corresponding sample size \code{n0}. If \code{NULL},
#'   then \code{\link{prior_ab}} will be called internally to calculate the
#'   prior.
#' @return a tibble containing each scenario associated with its power and type
#'   1 error
#' @export
#' @import foreach dplyr
#' @examples
#' temp1 <- pos_two_grid_search(p0 = 0.59, p1 = 0.812,
#'                 n0 = seq(50, 80, by =5),
#'                 cutoff = seq(0.5, 0.8, 0.02),
#'                 ncores = NA,
#'                 eval_success = TRUE)
#' @seealso \code{\link{pos_two_grid}}

pos_two_grid_search <- function(p0, p1, ...,
                                n0 = seq(50, 70, by = 5), n1 = n0,
                                cutoff = seq(0.05, 0.2, by = 0.05),
                                eval_success = TRUE, ncores = NA,
                                ab0 = NULL){
  # number of available cores
  aval_cores <- parallel::detectCores()
  if(is.na(ncores)){
    ncores <- aval_cores - 1
  } else if (ncores >= aval_cores){
    warning(paste0("Number of cores requested ", ncores, " is greater than
                   available ", aval_cores, ", will use ", aval_cores-1 , "instead."))
    ncores <- aval_cores - 1
  }

  # register for cluster computing
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl = cl)

  # prepare the search grid
  if(is.null(ab0)){
    ab0 <-  purrr::map(.x = n0, .f = prior_ab, p = p0) %>% bind_rows()
  }

  search_grid <- tibble::tibble(n0 = n0, n1 = n1) %>%
                  mutate(a0 = ab0$a, b0 = ab0$b)

  n_search <- nrow(search_grid)
  n_eval <- length(cutoff)

  result <- foreach::foreach(i = 1:n_search, .combine = "rbind",
                            # .export = c("pos_two_grid", "tail_prob", "prior_ab"),
                            .packages = c("dplyr")) %dopar% {

     param_i <- search_grid[i, ]

     ## since for each n1 & n2, the probabilities are fixed, therefore by comparing
     ## a single result to multiple thresholds, we can save computing time
     temp <- pos_two_grid(n0 = param_i$n0, n1 = param_i$n1, p0 = p0, p1 = p1,
                          ab0 = c(param_i$a0, param_i$b0), ...)
     # power
     prob_power <- purrr::map_dbl(.x = cutoff, .f = tail_prob, dat = temp,
                                  prob1 = prob_post, prob2 = prob_alt,
                                  eval_success = eval_success)
     # type 1 error
     prob_alpha <- purrr::map_dbl(.x = cutoff, .f = tail_prob, dat = temp,
                                  prob1 = prob_post, prob2 = prob_null,
                                  eval_success = eval_success)

     rslt <- tibble(n0 = param_i$n0, n1 = param_i$n1,
                    cutoff = cutoff, alpha = prob_alpha, power = prob_power,
                    a0 = unique(temp$a0), b0 = unique(temp$b0),
                    a1 = unique(temp$a1), b1 = unique(temp$b1))
     return(rslt)
   }
  return(result)
  # ensures "health" of future clusters
  doParallel::stopImplicitCluster()
}

