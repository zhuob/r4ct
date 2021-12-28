## sample size calculation using FM-score method (Farrington and Manning 1990)

# http://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Non-Inferiority_Tests_for_Two_Proportions.pdf
## this function is used to calculate sample size 

#' @title Farrington-Manning sample size calculation
#' @details this function is an implementation of Eq.(4) in Farrington &
#'   Manning, 1990 paper. The sample size calculation is based on \eqn{\hat{p}_1
#'   - \hat{p}_2 - \delta}, and the test has form 
#'   \deqn{H_0: p_1 - p_2 >=\delta VS H_1: p_1-p_2 < \delta } ,
#'   based on maximum likelihood estimation (Method 3 of that paper) under the
#'   null hypothesis restriction `p1_tilt - p2_tilt = delta`
# #'     | parameters |         |           |           |
# #'     |-----------|----------| ----------|-----------|
# #'     | Group     | Success  | Failure   |   Total   |
# #'     |  Treatment|   n11    |       n12 |         n1|
# #'     | Control   |  n21     |      n22  |        n2 |
# #'     | Total     |  n.1     |      n.2  |         n |
# #' 
# #' @md
#' 
#'  \itemize{
#'   \item{p10:}{group 1 proportion tested by the null }
#'   \item{delta:}{non-inferiority margin}
#'   \item{p1:}{binomial proportions = n11/n1}
#'   \item{p2:}{binomial proportions = n21/n2}
#'   \item{p:}{overall proportion  = m1/n}
#'   \item{test} {H0: p10 - p2 >= delta  vs  H1: p10 - p2 < delta}
#'  }
#'           
#'  Other forms of hypothesis test are also available. For more details please
#'  refer to the paper.
#' @references{
#' \insertRef{farrington1990test}{r4ct}
#' } 
#' 
#' @param p1 response probability in group 1
#' @param p2 response probability in group 2
#' @param theta randomization ratio (in the form of N2/N1)
#' @param delta the non-inferiority margin for risk difference under the null
#' @param alpha type 1 error control
#' @param beta 1- power
#' @param r0 the non-inferiority margin for the relative risk under the null
#' @param metric specifying the metric used to construct the test statistic,
#'   `riskdiff` for testing of risk difference (p1-p2) and `relriks` for testing
#'   relative risk (R = p1/p2)
#' @param alternative taking value of either `greater` for `delta <= delta0` vs
#'   `delta > delta0` or `two.sided` for `delta = delta0` vs `delta != delta0`.
#'   Similar for testing relative risk
#'
#' @return a table with sample size for each arm, and p1_tilt, p2_tilt that
#'   estimate p1 and p2 under the null hypothesis.
#' @export
#'
#' @examples
#' # reproducing first row of Table 1 in that paper
#' farrington_manning_n(p1 = 0.1, p2 = 0.1, r0 = 0.1, theta = 2/3, 
#'                metric = "relrisk", alpha = 0.05, beta = 0.1)
#' farrington_manning_n(p1 = 0.1, p2 = 0.1, delta = -0.2, theta = 2/3, 
#'                metric = "riskdiff", alpha = 0.05, beta = 0.1)

farrington_manning_n <- function(p1, p2, theta,  delta = NULL, r0 = NULL, 
                                 metric = "riskdiff", alpha = 0.05, beta = 0.2, 
                                 alternative = "greater"){

  # estimate p_tilt
  p_tilt <- prop_mle(p1_hat = p1, p2_hat = p2, delta = delta, theta = theta, 
                     r0 = r0, metric = metric)
  p1_tilt <- p_tilt[1]; p2_tilt <- p_tilt[2]
  # print(c(p1_tilt, p2_tilt))
  
  if(metric == "riskdiff"){
    # Note this formulae is a re-written of eq (4) in the paper, since the total 
    # sample size is returned with this approach: there is a mulitiplier (1+theta)
    se0 <- sqrt( (p1_tilt*(1-p1_tilt) + p2_tilt*(1-p2_tilt)/theta)*(1 + theta))
    se1 <- sqrt((p1*(1-p1) + p2*(1-p2)/theta)*(1 + theta))
    denom1 <- p1 - p2 - delta
    
  } else if(metric == "relrisk"){
    # Note this formulae is a re-written of eq (4) in the paper, since the total 
    # sample size is returned with this approach: there is a mulitiplier (1+theta)
    se0 <- sqrt((p1_tilt*(1-p1_tilt) + r0^2/theta * p2_tilt * (1 - p2_tilt))*(1 + theta))
    se1 <- sqrt( (p1*(1-p1) + r0^2/theta * p2 * (1-p2) )* (1 + theta) )
    denom1 <- p1-r0*p2
    
  }
  
  if(alternative == "greater"){
    # H0: delta <= delta0  or r <= r0
    z_alpha <- stats::qnorm(1 - alpha)
  } else if (alternative == "two.sided"){
    # H0: delta = delta0 vs H1: delta != delta0
    # or  H0: R = R0 vs H1: R != R0
    z_alpha <- stats::qnorm(1 - alpha/2)
  } 
  
  z_beta <- stats::qnorm(1 - beta)
  
  nsize <- ( (z_alpha*se0 + z_beta*se1)/denom1)^2
  # n1 <- nsize/(theta + 1); n2 <- nsize*theta/(theta + 1)
  # 
  # print(c(nsize*abs(denom1),  z_alpha*se0, se1))
  # pwr_beta <- stats::pnorm(((nsize*abs(denom1)) - z_alpha*se0)/se1)
  
  result <- tibble::tibble(n = nsize, n1 = nsize/(theta + 1), 
                           n2 = nsize*theta/(theta + 1),
                           p1_tilt, p2_tilt)
  
  
  return(result)
}



## estimate the MLE of two proportions using formula given by Farrington and Manning (see appendix)
prop_mle <- function(p1_hat, p2_hat, delta = NULL, r0 = NULL, theta, metric = "riskdiff"){
  
  # the boundary p values are handled based on Chan's 1999 paper: 
  # TEST-BASED EXACT CONFIDENCE INTERVALS FOR THE DIFFERENCES OF TWO BINOMIAL PROPORTIONS
    
    if(metric == "riskdiff"){
      if(is.null(delta)){
        stop("the non-inferiority margin 'delta' need to be specified!")
      }
      
      a <- 1 + theta
      b <- -(1 + theta + p1_hat + theta*p2_hat + delta*(theta + 2))
      c <- delta^2 + delta*(2*p1_hat + theta + 1) + p1_hat + theta*p2_hat
      d <- -p1_hat*delta*(1+delta)
      
      v <- (b/(3 * a))^3 - b * c/6/a^2 + d/2/a
      u <- (sign(v) + (v == 0)) * sqrt((b/3/a)^2 - c/3/a)
      
      # to avoid acos(1+1e-15) == NaN
      s <- v/(u^3)
      s[s>1] <- 1
      
      w <- (pi + acos(s))/3 
      
      p1_tilt <- 2*u*cos(w) - b/(3*a)
      p2_tilt <- p1_tilt - delta
      
      p2_tilt[p2_tilt < 0] <- 0  # 0.1 - 0.1 = -1.94289e-16
      
    } else if(metric == "relrisk"){
      if(is.null(r0)){
        stop("the non-inferiority margin 'r0' need to be specified!")
      }
      a <- 1 + theta
      b <- -(r0*(1 + theta*p2_hat) + theta + p1_hat)
      c <- r0*(p1_hat + theta*p2_hat)
      p1_tilt <- ( -b - sqrt(b^2 - 4*a*c) )/(2*a)
      p2_tilt <- p1_tilt/r0
    }
  
  
  return(c(p1_tilt, p2_tilt))
}



#' @title Farrington & Manning test statistic
#' @details test statistics by equation 1 of Farrington and Manning, 1990
#' this statistic is calculated based on the hypothesis that 
#'      H0: p1-p2 >= delta    vs    H1: p1 - p2 < delta
#' for the cases of (x1, x2) taking value of (0, 0), (0, 1), (1, 0), or (1, 1)
#' the boundary z values are handled based on Chan's 1999 paper
#' @references{
#' \insertRef{chan1999test}{r4ct}
#' }
#'
#' @param x1 the number of events in treatment
#' @param x2 the nubmer of events in control
#' @param n1 total number of events in treatment
#' @param n2 total number of events in control
#' @param delta non-inferiority margin for `risk difference`
#' @param r0 non-inferiority margin for `relative risk`
#' @param metric  specifying the metric used to construct the test statistic,
#'   `riskdiff` for testing of risk difference (p1-p2) and `relriks` for testing
#'   relative risk (R = p1/p2)
#'
#' @return the estimated p1_tilt, p2_tilt and test statistic
#' @export
#'
#' @examples
#'  farrington_manning_z(x1 = 10, x2 = 15, n1 = 40, n2 = 40, delta = 0.2)
#'  farrington_manning_z(x1 = 10, x2 = 15, n1 = 40, n2 = 40, r0 = 0.15, metric = "relrisk")
farrington_manning_z <- function(x1, x2, n1, n2, delta = NULL, r0 = NULL, metric = "riskdiff"){
  
  p1_hat <- x1/n1
  p2_hat <- x2/n2
  n <- n1 + n2
  theta <- n2/n1
  
  if( (p1_hat == 0 & p2_hat == 0 ) | (p1_hat == 1 & p2_hat == 1 ) ){
    z_eq <- 0
    p1_tilt <- p2_tilt <- 0
    s_eq <- 0
    }
  else {
    if(p1_hat ==0 & p2_hat == 1) { 
      
          
      p1_hat <- 1/(2*n1); p2_hat = 1- 1/(2*n2)
      } else if (p1_hat == 1 & p2_hat == 0){ 
        p1_hat <- 1- 1/(2*n1); p2_hat = 1/(2*n2) 
      }
  
  
    p_mle <- prop_mle(p1_hat = p1_hat, p2_hat = p2_hat, delta = delta, 
                      theta = theta, r0 = r0, metric = metric)
  
    p1_tilt <- p_mle[1]; 
    p2_tilt <- p_mle[2]
  
  
    if(metric == "riskdiff"){
    
      s_eq <- sqrt( (p1_tilt*(1-p1_tilt) + p2_tilt*(1-p2_tilt)/theta)*(1 + theta))/sqrt(n)
      z_eq <- (p1_hat-p2_hat - delta)/s_eq
    
    } else if (metric == "relrisk"){
     
      s_eq <- sqrt( (p1_tilt*(1-p1_tilt) + r0^2/theta * p2_tilt * (1 - p2_tilt))*(1 + theta) ) /sqrt(n)
      z_eq <- (p1_hat - r0/p2_hat)/s_eq
    }
  
  }
  
  
  return(tibble::tibble(p1=p1_tilt, p2= p2_tilt, s_eq = s_eq, z_eq = z_eq))
  
}


# the sum of probabilites whose combination of i and j results a test statistic
# that is less than or equal to z_eq
exact_prob <- function(n1, n2, p2, delta, z_index){
  
  d1 <- stats::dbinom(0:n1, size = n1, prob = delta + p2)
  d2 <- stats::dbinom(0:n2, size = n2, prob = p2)
  
  # probability matrix, prob_mat[i, j] = p(x = i|n1, p2 + delta)*p(y = j|n2, p2)
  prob_mat <- d1 %*% t(d2)  
  
  p_tail <- sum(prob_mat[z_index])
  
  return(p_tail)
  
}


# the z table for all possible combinations of x = i and y = j
# the z values are calculated based on FM-Score test.
farrington_manning_ztable <- function(n1, n2, delta){
  z_1 <- matrix(NA, nrow= n1 + 1, ncol = n2 + 1)
  
  for(i in 1:nrow(z_1)){  # calculate the z_eq for each pair of (i, j)
    for(j in 1:ncol(z_1)){
      temp <- farrington_manning_z(x1 = i-1, x2 = j-1, n1 = n1, n2 = n2, delta = delta)
      z_1[i, j] <- temp$z_eq
    }
    
  }
  
  return(z_1)
  
}



#' Significance level calculation using Farrington & Manning method
#'
#' @param n1 sample size in group 1
#' @param n2 sample size in group 2
#' @param delta non-inferiority margin
#' @param p2 the hypothesized response rate in group 2
#' @param alpha desired alpha level, e.g. 0.05
#' @param method one of `exact` or `normal`
#' \itemize{
#'   \item{`exact`}{ the test statistics are calculated using exact binomial distribution}
#'   \item{`normal`}{ the test statistics are derived based on normal approximation}
#' }
#' @import dplyr
#' @return numerical value of actual significance level.
#' @export
#'
#' @examples
farrington_manning_significance <- function(n1, n2, delta, p2, alpha = 0.05, method = "exact"){
  
  if(!(method %in% c("exact", "normal"))){
    stop("`method` must be one of 'exact', 'normal' ")
  }
  
  cumprob <- indicator <- NULL
  
  z1 <- farrington_manning_ztable(n1, n2, delta)
  
  if(method == 'exact'){
    # the joint probability for every combination of x=i and y = j;
    d1 <- stats::dbinom(0:n1, size = n1, prob = delta + p2)
    d2 <- stats::dbinom(0:n2, size = n2, prob = p2)
    # probability matrix, prob_mat[i, j] = p(x = i|n1, p2 + delta)*p(y = j|n2, p2)
    prob_mat <- d1 %*% t(d2) 
    
    # find the critical z value such that the tail is smaller than and 
    # as close as possible to the desired significance level
    z1_vec <- as.vector(z1)  # vectorize the table by column
    prob_vec <- as.vector(prob_mat)
    
    
    # find the more "extreme z values" such that their corresponding tail p value is <= alpha 
    ztab <- data.frame(z1_vec, prob_vec) %>% 
      arrange(z1_vec) %>% # sort the z values
      mutate(cumprob = cumsum(prob_vec)) %>%    # calculate cummulative probabilities
      mutate(indicator = cumprob <= alpha)      # decide a cut off z value
    
    # the cumprob in last row is the corresponding exact significant level
    z_critical <- ztab %>% filter(indicator == TRUE)          
    
    row_id <- nrow(z_critical)
    true_alpha <- z_critical[row_id, 3]
    
  } else if(method == "normal"){
    
    z1 <- farrington_manning_ztable(n1, n2, delta)
    z_critical <- stats::qnorm(alpha)
    z_index <- z1 <= z_critical  # the critical value if normal approximation is used
    
    # summing up the joint probabilities for all combinations of (i, j), such that z[i, j] < z_critical
    true_alpha <- exact_prob(n1, n2, p2, delta, z_index) 
    
  }
  
  return(true_alpha)
}

#' 
#' @title P value based on Chan's exact method
#' 
#' @details Calculating p values for based on Chan's exact method. max(p_exact)
#'   is the corresponding p-value 
#' @references{
#' \insertRef{chan1998exact}{r4ct}
#' }
#' @import tibble
#'
#' @param x1 number of response in group 1
#' @param x2 number of response in group 2 
#' @param n1,n2 number of subjects in group 1 and group 2, respectively
#' @param delta non-inferiority margin
#' @param p2_search search grid for p2, ranging from 0 to 1
#'
#' @return a tibble, each row represents a p2 and corresponding p value
#' @export
#'
#' @examples
#'  # this is the example given by Chan's 1998 paper (see figure 4)
#'  #
#'  x1 = 69; x2 = 83; n1 = 76; n2 = 88; delta = 0.1;
#'  p2_search <- seq(0.01, 0.9, by = 0.001)
#' pval <- farrington_manning_chan_pval(x1, x2, n1, n2, delta = delta,
#'  p2_search = p2_search)
#' max(pval$p_exact)

farrington_manning_chan_pval <- function(x1, x2, n1, n2, delta, p2_search){
  
  niter <- length(p2_search)
  z_test <- farrington_manning_z(x1, x2, n1, n2, delta)
  # calculate z for each combination of i and j
  z1 <- farrington_manning_ztable(n1 = n1, n2 = n2, delta =  delta)
  z_index <- z1 <= z_test   # a matrix of TRUE or FALSE 
  
  
  
  p_exact <- rep(NA, niter)
  for(i in 1:niter){
    p_exact[i] <- exact_prob(n1, n2, p2_search[i], delta = delta, z_index = z_index)
  }
  
  return(tibble::tibble(p2 = p2_search, p_exact = p_exact))
  
}



