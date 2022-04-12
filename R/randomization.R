
#' @title Generate block randomized arms
#' @param nsbj an integer for total number of subjects to be randominzed
#' @param ratio the allocation ratio with block size of \code{sum(ratio)}
#' @param arm_name a vector of characters for arms
#'
#' @return a vector of length `nsbj` with randomized treatment arms
#' @export
#'
#' @examples
#' rand_arm(nsbj = 1, ratio = c(1, 1))
#' rand_arm(nsbj = 12, ratio = c(2, 2, 1))
#' rand_arm(nsbj = 4, ratio = c(1, 2, 0, 1))
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
  arm_2 <- sample(rep(arm_name, ratio), n_remainder)
  arm <- c(arm_1, arm_2)
  return(arm)
}



#' Randomized arm given probability;
#'
#' @param nsbj number of subjects to be randomized
#' @param blocksize the block size?
#' @param prob a vector of randomization probabilities
#' @param arm_name a vector for the name of the arms
#'
#' @return simulated randomized arms
#' @export
#'
#' @examples
#' rand_arm_rar(10, blocksize = 5, prob = c(0.5, 0.5))
#' rand_arm_rar(10, blocksize = 5, prob = 0.5)
#' rand_arm_rar(10, blocksize = 5, prob = 2)

rand_arm_rar <- function(nsbj, blocksize, prob, arm_name = paste("arm", 1:length(prob), sep = "_")){

  if(any(prob < 0)){
    stop("vector prob cannot have negative number")
  }
  if(length(prob) == 1){
    if(prob > 1){
      warning("prob is not a vector and > 1, will be scaled and only 1 arm will be generated")
    } else{
    warning("prob is not a vector, only 1 arm will be generated")
    }
  } else{
    if(any(prob >= 1)){
      warning("elements in prob cannot be greater than 1, will be scaled to sum up to 1")
    } else if(sum(prob) != 1){
    message("vector prob does not sum up to 1, will be scaled to sum up to 1")
    }
  }
  nblock <- floor(nsbj/blocksize)
  nremained <- nsbj - nblock * blocksize


  # rmultinom will automatically scale probmax[2:4] so that they sum up to 1
  if(nblock > 0){
    nreps <- stats::rmultinom(n = nblock, size = blocksize, prob = prob)

    arm_block <- sapply(1:ncol(nreps), function(i){sample(rep(arm_name, nreps[, i]))})
    arm_block <- as.vector(arm_block)
  } else{arm_block <- NULL}

  if(nremained > 0){

    remained_alloc <- stats::rmultinom(n = 1, size = blocksize, prob = prob)
    arm_remained <- sample(rep(arm_name, remained_alloc), nremained)

    randarm <- c(arm_block, arm_remained)
  } else{
    randarm <- arm_block
  }

  return(randarm)
}





#' @title Generate subject level enrollment time by piecewise enrollment rate
#' @keywords internal
#'
#' @param nsbj number of subject enrolled
#' @param enrl_timecut a vector (or NA) specifying starting time for enrollment
#'   \itemize{
#'   \item{a vector}{indicating a piecewise enrollment rate; the vector always
#'   starts with 0}
#'   \item{NA      }{indicating a constant enrollment rate}
#'   }
#' @param enrl_rate a vector (or a single value) specifying the enrollment rate
#'   at each piece corresponding enrollment rate.
#'
#' @return a tibble where the first column is enrollment time, and the second
#'   column indicates the piece sequence
#' @export
#'
#' @examples
#' enrl_rate <- c(7, 14, 30)
#' enrl_timecut <- c(0, 1, 3)
#' timein1 <- rand_timein(nsbj = 300, enrl_timecut = enrl_timecut, enrl_rate = enrl_rate)
#'

rand_timein <- function(nsbj, enrl_timecut, enrl_rate){

  n_piece <- length(enrl_rate)
  if(n_piece != length(enrl_timecut)) {
    stop("# pieces for rate should be equal to # of pieces for start time")
  }
  if(length(enrl_timecut) > 1) {
    if(enrl_timecut[1] != 0)
      stop("start time should always begins with 0 when enrl_timecut is a vector")
  }
  else if(is.na(enrl_timecut)){
    enrl_timecut <- 0
  }
  ntemp <- nsbj;
  ## create a vector for enrollment endtime
  enrl_endtime <- c(enrl_timecut, 1e6)[-1]
  result <- NULL
  for (i in 1:n_piece){
    ## generate enroll time
    if(enrl_rate[i] == 0){ # if the enrollment speed is 0, do not generate timein
      temp_timein <- tibble::tibble(timein = NA, timein_piece = NA)
    } else{
      temp <- cumsum(stats::rexp(ntemp, enrl_rate[i])) + enrl_timecut[i]
      temp_timein <- tibble::tibble(timein = temp[temp <= enrl_endtime[i]], timein_piece = i)
    }

    ## keep only those within the specific time interval
    result <- dplyr::bind_rows(result, temp_timein)
    ntemp <- nsbj - nrow(temp_timein)
  }

  return(result)
}


rand_timeout_onepiece <- function(timein, drop_timecut, drop_prob){

  timein0 <- timein$timein
  timein_piece0 <- timein$timein_piece
  #dropout_dur <- c(droptime[1], diff(droptime))
  if(length(drop_timecut) != 1){
    stop("only one-piece dropout is allowed")
  }
  if(length(drop_timecut) != length(drop_prob)){
    stop("# of elements in droptime should be equal to # of elements in drop_prob")
  }

  nsbj <- length(timein0)
  dropout_hr <- -log(1-drop_prob)/drop_timecut

  sbj_drop_time <- stats::rexp(nsbj, rate = dropout_hr)

  result <- tibble::tibble(timein = timein0, timein_piece = timein_piece0,
                           timeout = sbj_drop_time, timeout_piece = 1)

  return(result)

}


#' Generate subject level enrollment/dropout time by piecewise enroll
#' rate/attrition probability
#' @details This function generates enrollment time and dropout time based on
#'   exponential distribution.
#'   \itemize{
#'   \item{piecewise enrollment time}{under exponential distribution, if it's a
#'   piecewise enrollment, here are the steps: (1) for each piece, sequentially
#'   generate a poission number N to reprensent the number of subjects enrolled
#'   for that piece; (2) generate a vector  of length N from exponential
#'   distribution, representing the enrollment time; (3) combine the pieces
#'   together.
#'   }
#'   \item{dropout time}{Currently only support one-piece dropout!  Algorithm:
#'   (1) given the dropout probability and dropout cut time, calcluate the
#'   dropout hazard ratio by \code{dropout_hr <-
#'   -log(1-drop_prob)/drop_timecut}; (2) generate dropout time from exponential
#'   distribution with \code{rate = dropout_hr}.
#'   }
#'}
#' @param nsbj number of subjects to generate
#' @param enrl_timecut a vector (or NA) specifying starting time for enrollment
#'   \itemize{
#'   \item{a vector}{indicating a piecewise enrollment rate; the vector always
#'   starts with 0}
#'   \item{NA      }{indicating a constant enrollment rate}
#'   }
#' @param enrl_rate a vector (or a single value) specifying the enrollment rate
#'   at each piece corresponding enrollment rate.
#' @param drop_timecut time cutoff for drop out; if \code{drop_timecut = NA}, t
#'   means no dropout is simulated.
#' @param drop_prob dropout probability by \code{drop_timecut}
#'
#' @return a tibble with columns of timein and timeout; Note the dropout time
#'   generated is relative to timein, NOT the calendar time.
#' @export
#'
#' @examples
#' inout <- rand_inout(nsbj = 300, enrl_timecut = c(0, 3, 7), enrl_rate = c(2, 5, 8),
#' drop_timecut = 12, drop_prob = 0.1)
#'


rand_inout <- function(nsbj, enrl_timecut, enrl_rate,
                       drop_timecut = NA, drop_prob = NA){

  timein1 <- rand_timein(nsbj = nsbj, enrl_timecut = enrl_timecut, enrl_rate = enrl_rate)

  if(!is.na(drop_timecut)){

    timeout1 <- rand_timeout_onepiece(timein = timein1, drop_timecut = drop_timecut,
                             drop_prob = drop_prob)
  }
  if(is.na(drop_timecut)){
    message("dropout info is partially specified, assuming no dropout\n")
    result <- timein1}
  else if (!is.na(drop_timecut)){ result <- timeout1}

  return(result)
}

# timein1 <- rand_timein(nsbj = 10000, enrl_timecut = c(0,3,7), enrl_rate = c(5, 10, 20))
# timeout <- rand_inout(timein1$timein, droptime = c(6, 12), drop_prob = c(0.1, 0.15))
# c(mean(timeout$timeout < 6), mean(timeout$timeout < 12));
# table(timeout$piece)
# max(timein1$timein)
