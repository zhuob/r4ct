library(dplyr)

testthat::test_that( "randomized enrollment is done correctly",{

  rate <- c(7e3, 1e4, 5e4); starttime <- c(0, 1.5, 2.5)
  timein1 <- rand_timein(nsbj = 1e5, rate = rate, starttime = starttime)

  x1 <- mean(diff(timein1$timein[timein1$piece == 1]));
  x2 <- mean(diff(timein1$timein[timein1$piece == 2]));
  x3 <- mean(diff(timein1$timein[timein1$piece == 3]));
  x <- c(x1, x2, x3)
  expect_equal(sum(abs(x - 1/rate)*rate < 5e-2), 3)
  }
)
