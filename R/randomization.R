
#' @title generate block randomized arms
#'
#' @param nsbj an integer for total number of subjects to be randominzed
#' @param ratio the allocation ratio
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



#' Generate enrollment time by piecewise enrollment rate
#'
#' @param nsbj number of subject enrolled
#' @param rate a vector (or a single value) specifying the enrollment rate at
#'   each piece
#' @param starttime a vector (or a single value) specifying starting time for
#'   corresponding enrollment rate. \code{starttime} always starts with 0,
#'   whether it's a vector or a single value.
#'
#' @return a tibble where the first column is enrollment time, and the second
#'   column indicates the piece sequence
#' @export
#'
#' @examples
#' rate <- c(7, 14, 30)
#' starttime <- c(0, 1, 3)
#' timein1 <- rand_timein(nsbj = 300, rate = rate, starttime = starttime)
#'

rand_timein <- function(nsbj, rate, starttime){

  n_piece <- length(rate)
  if(n_piece != length(starttime)) {
    stop("# pieces for rate should be equal to # of pieces for start time")
   }

  ntemp <- 0; timein <- NULL
  piece <- NULL
  # this is to make sure that the enrollment time ends until nsbj are enrolled
  enrl_dur <- c(diff(starttime), 1e6)
  for (i in 1:n_piece){
    # min(rate[i]*enrl_dur[i], 1e7) to avoid an error from rpois()
    n1 <- min(rpois(1, min(rate[i]*enrl_dur[i], 1e7)), nsbj - ntemp)
    ntemp <- ntemp + n1;
    piece <- c(piece, rep(i, n1))
    last_piece_end_time <- ifelse(is.null(timein), 0, max(timein))
    timein <- c(timein, last_piece_end_time + cumsum(rexp(n1, rate[i])))
  }

  return(tibble::tibble(timein = timein, piece = piece))
}



