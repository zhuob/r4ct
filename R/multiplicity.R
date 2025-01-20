#' Perform truncated Hochberg procedure 
#'
#' @param pvals a vector of p-values to be adjusted for multiplicity 
#' @param truncation_gamma the truncation parameter between 0 and 1, with 0 
#' being Bonferroni procedure and 1 corresponding to Hochberg procedure. A 
#' larger value yields a larger alpha reserved if one or more test fails
#' @param overall_alpha the overall alpha level.
#'
#' @return a tibble including the outcome of each test as well as alpha reserved
#' @export
#'
#' @examples
#' test_truncated_hochberg(c(0.01, 0.026), 0.8, overall_alpha = 0.025)
test_truncated_hochberg <- function(pvals, truncation_gamma, overall_alpha){
  
  # number of tests to be adjusted 
  n_test <- length(pvals)
  # calculate significant levels for the tests
  step_down_part <- truncation_gamma / (n_test - 1:n_test + 1) 
  bonferroni_part <- (1 - truncation_gamma)/n_test
  test_levels <- (step_down_part + bonferroni_part) * overall_alpha
  
  rank_pvals <- rank(pvals)
  test0 <- sort(pvals) < test_levels
  positve_test <- which(test0)
  n_positive <- sum(test0)
  test_positive <- rank_pvals <= n_positive
  test_result <- matrix(test_positive, nrow = 1)
  colnames(test_result) <- paste0("test", 1:n_test)
  
  test_result <- tibble::as_tibble(test_result)
  
  truncated_alpha <- (1-truncation_gamma) * (1-n_positive/n_test) * overall_alpha
  alpha_preserved <- dplyr::if_else(
    n_positive == 0, 0, dplyr::if_else(
      n_positive == n_test, overall_alpha, truncated_alpha
    ))
  
  alpha_preserved <- dplyr::if_else(
    truncation_gamma == 1 & n_positive != n_test, 0, alpha_preserved)
  
  test_result$alpha_preserved <- alpha_preserved
  
  return(test_result)
    
}
