test_that("testing function 'farrington_manning_significance' for Significance level calculation using Farrington & Manning method ", {
  
  pval <- farrington_manning_significance(n1 = 50, n2 = 50, delta = 0.2, p2 = 0.7, alpha = 0.05)
  
  expect_equal(pval, 0.047598, tolerance = 1e-3)
})


test_that("Testing function 'farrington_manning_n' for sample size calculation", {
  
  # test sample size calculation using relative risk
  n_test1 <- farrington_manning_n(p1 = 0.1, p2 = 0.1, r0 = 0.1, theta = 2/3, 
                             metric = "relrisk", alpha = 0.05, beta = 0.1)

  expect_equal(round(c(n_test1$n1, n_test1$n2)), c(49, 33))
  
  # test sample size calculation using risk difference
  n_test2 <- farrington_manning_n(p1 = 0.1, p2 = 0.1, delta = -0.2, theta = 2/3, 
                       metric = "riskdiff", alpha = 0.05, beta = 0.1)
  
  expect_equal(round(c(n_test2$n1, n_test2$n2)), c(63, 42))
  
  
  
})


test_that("Testing 'farrington_manning_chan_pval' for P value based on Chan's exact method", {
  
  
  # verify the first example in Chan's 1998 paper
  x1 = 69; x2 = 83; n1 = 76; n2 = 88; delta = 0.1;
  p2_search <- seq(0.01, 0.9, by = 0.001)
  pval <- farrington_manning_chan_pval(x1, x2, n1, n2, delta = delta, p2_search = p2_search)
  p_target <- max(pval$p_exact)
  expect_equal(p_target, 0.0017, tolerance = 5e-3)
  expect_equal(pval$p2[pval$p_exact == p_target], 0.441, tolerance = 1e-2)
  
  
})