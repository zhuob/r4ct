
#' @title generate blocked randomized arms
#'
#' @param nsbj a vector specifying number of subjects to be randominzed for each
#'   arm
#' @param ratio the allocation ratio
#'
#' @return a vector of length `sum(nsbj)` with randomized treatment arms
#' @export
#'
#' @examples
#' rand_arm(nsbj = c(0, 1), ratio = c(1, 1))
#' rand_arm(nsbj = c(5, 5, 2), ratio = c(2, 2, 1))
#'
rand_arm <- function(nsbj, ratio){

  n_b <- nsbj / ratio;
  n_block <- floor(min(n_b))
  n_remainder <- nsbj - n_block*ratio
  n_arm <- length(nsbj)
  arm_name <- rep(paste("arm", 1:n_arm, sep = "_"), ratio)
  if(n_block == 0){
    arm_1 <- NULL
  } else{
  arm_1 <- as.vector(replicate(n_block, sample(arm_name)))
  }
  arm_2 <- rep(paste("arm", 1:n_arm, sep = "_"), n_remainder)
  arm_2 <- sample(arm_2) ## shuffle it
  arm <- c(arm_1, arm_2)
  return(arm)
}
