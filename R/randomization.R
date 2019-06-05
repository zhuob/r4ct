
#' @title generate blocked randomized arms
#'
#' @param nsbj an integer for total number of subjects to be randominzed
#' @param ratio the allocation ratio
#' @param arm_name a vector of characters for arms
#'
#' @return a vector of length `sum(nsbj)` with randomized treatment arms
#' @export
#'
#' @examples
#' rand_arm(nsbj = 1, ratio = c(1, 1))
#' rand_arm(nsbj = 12, ratio = c(2, 2, 1))
#'
rand_arm <- function(nsbj, ratio, arm_name = paste("arm", 1:length(ratio), sep = "_")){

  n_arm <- length(ratio)
  n_block <- floor(nsbj / sum(ratio))
  n_per_arm <- rep(n_block, n_arm)
  n_remainder <- nsbj - sum(n_per_arm * ratio)
  arms <- rep(arm_name, ratio)

  if(n_block == 0){
    arm_1 <- NULL
  } else{
  arm_1 <- as.vector(replicate(n_block, sample(arms)))
  }
  arm_2 <- sample(arm_name, n_remainder)
  arm <- c(arm_1, arm_2)
  return(arm)
}
