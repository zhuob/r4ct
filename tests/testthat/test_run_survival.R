#test function: run_survival

testthat::test_that("run survival analysis on time to event data", {

  time <- c(5.68, 0.34, 4.94, 1.49, 4.72, 1.32, 3.48, 3.42, 3.41, 2.93, 0.22, 2.35,
            0.61, 1.98, 0.98, 1.62, 1.19, 1.18, 0.97, 0.90, 0.80, 0.45, 0.02)
  censor <- c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
  arm <- c("arm_1", "arm_2", "arm_1", "arm_2", "arm_2", "arm_1", "arm_2", "arm_1", "arm_1", "arm_2", "arm_1", "arm_2",
           "arm_1", "arm_2", "arm_1", "arm_2", "arm_2", "arm_1", "arm_1", "arm_2", "arm_1", "arm_2", "arm_1")
  control <- "arm_1"

  res1 <- run_survival(time, censor, arm, control = "arm_1")
  testthat::expect_equal(sum(res1$nsubj), length(time), tolerance = 0);
  testthat::expect_equal(sum(res1$nevent), sum(censor==1), tolerance = 0);
  
  # test p value matches with HR direction
  control <- "arm_2"
  res2 <- run_survival(time, censor, arm, control = control)
  
  testthat::expect_equal(unique(res2$p_logrank), 1 - 0.232, tolerance = 1e-2)
  
}
)

