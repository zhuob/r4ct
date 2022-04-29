# library(dplyr)

testthat::test_that( "randomized enrollment/dropout is done correctly",{

  ########## Test by Developer
  # case:   given specified piecewise enrollment rate, it can generates timein as
  #         expected
  # logic:  For a given vector of enrollment rates, if we take lagged and
  #         iterated difference (i.e., diff() in R) by enrollment piece, the
  #         resulting data should follow exponential distribution with rates
  #         specified
  # result: the (by piece) mean of the resulting vector should match 1/rate
  set.seed(1234)
  rate <- c(7, 10, 50); starttime <- c(0, 5e3, 1e4)
  suppressMessages(timein1 <- rand_inout(nsbj = 1e5, enrl_timecut = starttime, enrl_rate = rate,
                        drop_timecut = NA, drop_prob = NA))

  x1 <- mean(diff(timein1$timein[timein1$timein_piece == 1]));
  x2 <- mean(diff(timein1$timein[timein1$timein_piece == 2]));
  x3 <- mean(diff(timein1$timein[timein1$timein_piece == 3]));
  x <- c(x1, x2, x3)
  testthat::expect_equal(sum(abs(x - 1/rate)*rate < 5e-2), 3)

  ########## Experienced by Xiaoyue, Fixed by Bin
  # Case:  the initial rate is small, which may result in 0 subjects for the
  #        first piece of enrollment. Subsequently, it causes the original code
  #        to break, generating -Inf timein
  # Logic: if we let the first piece of rate to be 0, it can test whether this
  #        function can handle the issue
  # Result: the returned timein should be all positive and finite
  set.seed(5678)
  x4 <- suppressMessages(rand_inout(nsbj = 200, enrl_rate = c(0, 10), enrl_timecut = c(0, 6),
                    drop_timecut = NA, drop_prob = NA))
  testthat::expect_equal(sum(is.infinite(x4$timein)), 0)


  ########## Need to think of a case to verify the random dropout time
  # one piece dropout
  set.seed(9101112)
  suppressMessages(timeout1 <- rand_inout(nsbj = 3e5, enrl_timecut = 0,
                        enrl_rate = 1.25e4,
                        drop_timecut = 12,
                        drop_prob = 0.10))

  testthat::expect_equal(mean(timeout1$timeout < 12), 0.1, tolerance = 1e-2);

  ####  add a test to check the number of subjects generated at a given time
  ## interval is correct
  enrl_timecut = c(0, 1)
  enrl_rate = c(1, 50 )

  pr1 <- c()
  set.seed(12345)
  for(i in 1:1000){
    suppressMessages(x1 <- rand_inout(nsbj = 1e4, enrl_timecut = enrl_timecut, enrl_rate = enrl_rate))
    pr1[i] <- sum(x1$timein < enrl_timecut[2])
  }
  testthat::expect_equal(mean(pr1), 1, tolerance = 1e-1)

  }
)

testthat::test_that("randomized arm is done correctly", {

  x1 <- rand_arm(nsbj = 4, ratio = c(1, 1), arm_name = 1:2)
  x2 <- rand_arm(nsbj = 4, ratio = c(1,2,0,1), arm_name = c("A", "B", "C", "D"))
  x3 <- rand_arm(nsbj = 12, ratio = c(2, 2, 2), arm_name = 1:3)


  testthat::expect_equal(sum(x1 == 2), 2);
  testthat::expect_equal(sum(x2 == "C"), 0);
  testthat::expect_equal(sum(x3 == 3), 4);

  # Tony Jiang
  # 13-sep-2019
  # test rand_arm

  set.seed(123)
  x <- rand_arm(100000, ratio = c(2, 3), arm_name = c("pbo", "trt"))
  x1 <- x[1:5]
  x2 <- x[6:10]
  testthat::expect_equal(as.numeric(table(x1)), c(2, 3))
  testthat::expect_equal(as.numeric(table(x2)), c(2, 3))
  testthat::expect_equal(as.numeric(table(x)/100000), c(0.4, 0.6), tolerance = 1e-3)
  }
)




testthat::test_that("randomized arm with probability is done correctly", {


  ############ Developer test ###############################
  # 2019-11-11, Bin Zhuo

  ## do not allow negative prob
  testthat::expect_error(rand_arm_rar(nsbj = 10, blocksize = 2, prob = c(-0.2, 0.3)))
  ## if prob is a scalar, issue a warning
  testthat::expect_warning(rand_arm_rar(nsbj = 10, blocksize = 5, prob = 0.2))
  ## if prob does not sum up to 1, issue a warning and re-scale prob
  testthat::expect_message(rand_arm_rar(nsbj = 10, blocksize = 5, prob = c(0.2, .3)))
  ## if prob has elements > 1, issue a warning
  testthat::expect_warning(rand_arm_rar(nsbj = 10, blocksize = 2, prob = c(0.2, 1.2)))

  # is the probability correct?
  nsim <- 1e5; prob0 <- c(0.5, 0.2, 0.3)
  x1 <- table(rand_arm_rar(nsbj = nsim, blocksize = 10, prob = prob0))/nsim
  testthat::expect_equal(as.vector(x1), prob0, tolerance = 1e-2)

}
)


