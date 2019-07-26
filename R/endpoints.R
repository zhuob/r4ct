

#' @title generate correlated endpoints of PFS and OS
#' @details This function generates correlated PFS and OS based on a paper
#'   published in 2009. Specifically, Let
#'   \eqn{X_1 \sim \exp(\lambda_1), X_2 \sim \exp(\lambda_2), X_3 \sim \exp(\lambda_3)},
#'   where \eqn{X_i} has pdf \eqn{f_{X}(x_i) = \lambda_i\exp(-\lambda_ix_i)}.
#'   \itemize{
#'    \item{Theorem 1.}{ If \eqn{PFS = \min(X_1, X_2)}, \eqn{OS = X_2} then \deqn{Corr(PFS, OS) = \frac{\lambda_2}{\lambda_1 + \lambda_2}}}
#'    \item{Theorem 2.}{ If we define \eqn{PFS = \min(X_1, X_2)}, \eqn{OS = PFS} if \eqn{PFS = X_2} and \eqn{OS = X_1 + X_3} otherwise, then \deqn{Corr(PFS, OS) = \frac{\lambda_3}{\sqrt{\lambda_1^2 + 2\lambda_1\lambda_2 + \lambda_3^2}} }}
#'   }
#' @references{
#'   \insertRef{fleischer2009statistical}{MethodDev}
#' }
#'
#' @param nsbj  number of subjects to be simulated
#' @param mos median overall survival time
#' @param mosp median overall survival since progression. If \code{mosp = NA},
#'   then OS and PFS will be generated using Theorem 1. If a numerical value is
#'   assigned to \code{mosp}, then OS and PFS will be generated using Theorem 2.
#' @param mpfs median PFS
#'
#' @return a tibble with correlated PFS and OS
#' @export
#' @examples
#' mos <- 6.7; mosp  <- 5.4; mpfs <- 1.93
#' y1 <- gen_ospfs(nsbj = 1e5, mos = mos, mosp = mosp, mpfs = mpfs)
#' cor(y1)
#' y2 <- gen_ospfs(nsbj = 1e5, mos, mosp = NA, mpfs)
#' all.equal(cor(y2)[1,2],mpfs/mos, tolerance = 1e-3)

gen_ospfs <- function(nsbj, mos, mosp = NA, mpfs){

  if(!is.na(mosp)){

  lambda <- compute_lambda(os = mos, osp = mosp, pfs = mpfs)

  rttp <- rexp(nsbj, lambda[1])
  rx <- rexp(nsbj, lambda[2] + 1e-6)
  rosp <- rexp(nsbj, lambda[3])

  pfs <- pmin(rttp, rx)
  os <- ifelse(rx < rttp, rx, rttp + rosp)

  } else{

    lambda2 <- log(2)/mos
    lambda1 <- log(2)/mpfs - lambda2
    rttp <- rexp(nsbj, lambda1)
    os <- rexp(nsbj, lambda2)
    pfs <- pmin(rttp, os)
  }

  return(tibble::tibble(pfs = pfs, os = os))
}


#' Calculate \eqn{\lambda}'s based on the formula
#'
#' @param os median os
#' @param osp median OS after progression
#' @param pfs median PFS
#'
#' @return a vector of lambda's and the theoretical correlation between OS and PFS
#'
compute_lambda <- function(os, osp, pfs){

  lambda3 <- log(2)/osp

  get_lambda1 <- function(mos, mosp, mpfs, lambda1){

    t1 <- lambda1*exp(-log(2)/mosp*mos) / (log(2)*(1/mpfs - 1/mosp))
    t2 <- (log(2)*(1/mpfs - 1/mosp) + lambda1)/(log(2)*(1/mpfs - 1/mos)) * exp(-log(2)/mpfs*mos)
    1-t1 + t2 - 1/2

  }
  p1 <- uniroot(function(x) get_lambda1(mos = os, mosp = osp, mpfs = pfs, x), c(0, 1e3))

  lambda1 <- p1$root
  lambda2 <- max(log(2)/pfs - lambda1, 0)
  lambda3 <- log(2)/osp
  ospfscor <-  lambda3/sqrt(lambda1^2 + 2*lambda2*lambda3 + lambda3^2)

  result <- c(lambda1, lambda2, lambda3, ospfscor)
  names(result) <- c("lambda1", "lambda2", "lambda3", "cor")
  return(result)
}

