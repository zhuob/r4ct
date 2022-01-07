
# calculate decision matrix 

divide_interval <- function(tolerance1, tolerance2, target, method = "mtpi2"){
  
  
  pt1 <- target - tolerance1 
  pt2 <- target + tolerance2	## target toxicity
  
  if(method == "mtpi"){
    pvi <- c(0, pt1, pt2, 1)
    
  } else if (method == "mtpi2"){
    pwdi <- tolerance1 + tolerance2					   ## START of modification for mTPI2
    li   <- c(seq(pt1, 0, by = -1 * pwdi), 0)  # the subintervals below EI (equivalence interval)
    hi   <- c(seq(pt2, 1, by = pwdi), 1)       # the subintervals above EI
    pvi  <- sort(unique(c(li, hi)))
    
  }
  
  pcat <- cut(pvi, breaks = c(-1, pt1, pt2, 1), labels = c("LI", "EI", "HI"))[-1]
  
  return(list(pvi = pvi, pcat = pcat))
  
}

#' @title Bayes Decision Table using modified Toxicity Probability interval (mTPI)
#' @details For each cohort size and number of DLTs observed, calculate the
#'   Bayes posterior probability, based on which a decision table is generated
#'   using mTPI2 (or mTPI) method.
#' @references{
#'   \insertRef{guo2017bayesian}{r4ct}
#' }
#'   
#' @importFrom Rdpack reprompt
#' @import magrittr
#'
#' @param cocap Cohort Cap, max number of subjects in a cohort
#' @param target Target Toxicity Rate
#' @param tolerance1 Equivalence Radius -(target tox - tolerance1 = lower acceptance value )
#' @param tolerance2 Equivalence Radius +(target tox - tolerance2 = upper acceptance value)
#' @param a parameter for beta distribution prior
#' @param b parameter for beta distribution prior
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#' @param method either `mtpi` using mTPI method, or `mtpi2` using mTPI2 method
#'
#' @return a decision matrix with rows being the number of toxicities, and
#'   column the corresponding number of subjects in that cohort
#' @export
#'
#' @examples
#' # create decision table in figure 3
#' x1 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi")
#' x2 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi2")
#' # create decision table in figure C1 of the mTPI2 paper
#' x3 <- mtpi2_decision_matrix(cocap = 10, target = 0.1, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.02, tox = 0.95, method = "mtpi")
#' x4 <- mtpi2_decision_matrix(cocap = 10, target = 0.1, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.02, tox = 0.95, method = "mtpi2")
#' 
mtpi2_decision_matrix <- function(cocap, target, a, b, tolerance1, tolerance2, tox, method = "mtpi"){
  
  get_interval <- divide_interval(tolerance1, tolerance2, target, method = method)
  pvi <- get_interval$pvi
  pcat <- get_interval$pcat # 2 means EI
  
  
  decision_info <- list()
  # number of toxicities observed
  decision_info$ntox  <- matrix(0:cocap, nrow = cocap + 1, ncol = cocap)
  # number of subjects 
  decision_info$nsubj <- matrix(1:cocap, nrow = cocap + 1, ncol = cocap, byrow = TRUE)
  # non-existing combos
  non_compatible <- decision_info$ntox > decision_info$nsubj 
  #decision_info$ntox[non_compatible]  <- NA
  decision_info$nsubj[non_compatible] <- 999
  
  # posterior parameters based on beta-binomial
  decision_info$posta <- a + decision_info$ntox
  decision_info$postb <- b + decision_info$nsubj - decision_info$ntox
  
  
  # unite probability mass calculation
  n_interval <- length(pvi) -1
  upm_by_interval <- array(NA, dim = c(n_interval, cocap + 1, cocap))
  pdel <- diff(pvi)
  
  for(i in 1:n_interval){
    
    prob1 <-  stats::pbeta(pvi[i],   decision_info$posta, decision_info$postb)
    prob2 <-  stats::pbeta(pvi[i+1], decision_info$posta, decision_info$postb)
    prob_diff <- prob2 - prob1
    upm_by_interval[i, , ] <- prob_diff / pdel[i]
    
  }
  
  # find the location of max upm for each cell
  max_upm_cell <- apply(upm_by_interval, c(2, 3), which.max)
  
  # assign D/E/S levels to each cell
  dtab <- matrix(NA, nrow = cocap + 1, ncol = cocap)
  target_tox_interval <-  which(pcat == "EI")
  dtab[max_upm_cell < target_tox_interval] <- "E"
  dtab[max_upm_cell > target_tox_interval] <- "D"
  dtab[max_upm_cell == target_tox_interval] <- "S"
  
  # decide unacceptable toxicity cells
  # Post.Prob(p > target | data) > tox; then DU
  untox_cell <- stats::pbeta(target, decision_info$posta, decision_info$postb, lower.tail = FALSE) > tox
  # Do not apply stoping rules until >= 3 patients have been evaluated at a dose
  untox_cell[, 1:2] <- FALSE
  dtab[untox_cell] <- "DU"
  dtab[non_compatible] <- NA
  
  decision_info$decision_table <- dtab
  return(dtab)
  
}


#' Plot the decision matrix for DLT
#'
#' @param dtab a decision matrix with columns being the number of subjects, and
#'   rows representing corresponding DLTs.
#' @rdname mtpi2_decision_matrix
#' @return a ggplot object
#' @export
#' @examples
#' x1 <- mtpi2_decision_matrix(cocap = 10, target = 0.3, a = 1, b = 1, 
#'    tolerance1 = 0.05, tolerance2 = 0.05, tox = 0.95, method = "mtpi")
#'    plot_decision_matrix(x1)
plot_decision_matrix <- function(dtab){
  
  ndlt <- decision <- NULL
  # the maximum number of subjects in a cohort
  cocap <- ncol(dtab)
  
  # Find the starting row that will not display (because all are "DU")
  for(i in 1:(cocap+1)) {
    if(is.na(any(dtab[i,]=="D" | dtab[i,]=="S" | dtab[i,]=='E')==FALSE)) {
      cut <- i+1
      break
    } 
  }
  sub_dtab <- dtab[-(cut:(cocap + 1)), ]
  ntox <- rep(0:(nrow(sub_dtab)-1), ncol(sub_dtab))
  nsubj <- rep(1:ncol(sub_dtab), each = nrow(sub_dtab))
  sub_dtab_reform <- tibble::tibble(ndlt = ntox, nsubj, decision = as.vector(sub_dtab)) %>%
    dplyr::filter(!is.na(decision)) %>% 
    dplyr::mutate(nsubj = factor(nsubj, levels = unique(nsubj)), 
                  ndlt = factor(ndlt, levels = unique(ntox)))
  
  ## generate the decision table
  fig <- ggplot2::ggplot(data = sub_dtab_reform, ggplot2::aes(x = nsubj, y = ndlt, fill = decision)) + 
    ggplot2::geom_tile(color = "white") + 
    ggplot2::scale_y_discrete(limits = rev(levels(sub_dtab_reform$ndlt))) + 
    ggplot2::scale_x_discrete(position = "top") + 
    ggplot2::scale_fill_manual(values = c("#0063c3", "#d34d2f",  "#88c765", "#ec951a"), 
                               labels = c("D", "DU", "E", "S")) + 
    ggplot2::theme(legend.position = "bottom", 
                   axis.text = ggplot2::element_text(face = "bold", size = 15), 
                   axis.title = ggplot2::element_text(face = "bold", size = 20), 
                   plot.title = ggplot2::element_text(face = "bold", size = 25)
    ) + 
    ggplot2::labs( x = "Number of Subjects at Current Dose", y = "Number of DLTs")
  
  return(fig)
  
}



########################################################################################
## plot
########################################################################################

#' @title Bayes Decision Table for mTPI2
#' @details For each cohort size and number of DLTs observed, calculate the
#'   Bayes posterior probability, based on which a decision table is generated
#'   using mTPI2 method.
#' @references{
#'   \insertRef{guo2017bayesian}{r4ct}
#' }
#'   
#' @importFrom Rdpack reprompt
#' @import magrittr
#' @param nmax Maximum Sample Size for the Trial
#' @param cocap Cohort Cap, max number of subjects in a cohort
#' @param tolerance1 Equivalence Radius -(target tox - tolerance1 = lower acceptance value )
#' @param tolerance2 Equivalence Radius +(target tox - tolerance2 = upper acceptance value)
#' @param a parameter for beta distribution prior
#' @param b parameter for beta distribution prior
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#' @param target Target Toxicity Rate
#'
#' @return a decision plot
#' @export
#'
#' @examples
#' mtpi2_decision_plot(nmax = 30, cocap = 12, tolerance1 = 0.05, tolerance2 =
#' 0.05, a = 1, b = 1, tox = 0.95, target = 0.3)
mtpi2_decision_plot = function(nmax, cocap, tolerance1, tolerance2, a, b, tox, target)
{
  
  #   no visible binding for global variable 'nsubj'
  nsubj <- No.of.DLTs  <- ndlt <- decision <- NULL
  
  mxn <- nmax
  mxn1 <- mxn+1						## max N
  ncohd0 <- cocap	
  
  pt <- target
  pt1 <- pt-tolerance1 
  pt2 <- pt+tolerance2	## target toxicity
  
  pwdi <- tolerance1+tolerance2						## START of modification for mTPI2
  pvi <- c(0,pt1-99:0*pwdi,pt2+0:99*pwdi,1)
  pvi <- unique(pvi[0<=pvi&pvi<=1]) 
  npvi <- length(pvi)
  pdel <- diff(pvi)									
  pcat <- as.integer(cut(utils::head(pvi,-1)+pdel/2,c(-1,pt1,pt2,2)))	## END of modification
  
  apr <- a 
  bpr <- b					## hyper a b
  p.ud <- tox
  FF <- c(".","E","S","D","DU")
  
  ## Produce Dose-Finding Spreadsheet
  am=array(0:mxn,c(mxn1,mxn)) 
  bm=t(array(1:mxn,c(mxn,mxn1)))
  abet=am+apr 
  bbet=bm-am+bpr 
  bbet[bbet<=0]=999		## beta a b - 999 for not existing comb
  
  pbet=NULL										## START of modification for mTPI2
  for(i in 1:npvi) pbet=cbind(pbet,c(stats::pbeta(pvi[i],abet,bbet)))
  rowdiffpbet <- pbet[, 2:ncol(pbet)] - pbet[, 1:(ncol(pbet) - 1)]
  UPM0=t(t(rowdiffpbet)/pdel)
  UPM0=UPM0[,c(2:npvi-1,1,which.max(pvi==pt1),npvi-1)]	## preventing length(vector)=1
  UPM=NULL; for(i in 1:3) UPM=cbind(UPM,apply(UPM0[,c(pcat,1:3)==i],1,max))
  UPM=array(UPM,c(mxn1,mxn,3))						## END of modification
  
  Escf=apply(UPM,1:2,which.max) 
  Escf[bbet==999]=0	## unit prob mass; esc, stay, de-esc
  upr=1-stats::pbeta(pt,abet,bbet) 
  Udos=1*(Escf==3&upr>p.ud) ## unacceptable tox
  Escf=Escf+Udos; 
  
  if (ncohd0<mxn) {
    for (ib in ncohd0:mxn)	{
      for (jb in 1:mxn1)	Escf[jb,ib]=ifelse(Escf[jb,ib]==2,2*((jb-1)/ib>pt)+1,Escf[jb,ib])
    }
  }
  
  GG=cbind(0:mxn,as.data.frame(matrix(FF[Escf+1],mxn1,mxn)))	## Coded escalation spreadsheet
  colnames(GG)=c("No.of.DLTs",paste("n=",1:mxn,sep=""))
  Escf=matrix(match(as.matrix(GG)[,-1],FF),mxn1,mxn)-1	## EscalationTable
  
  xx <- GG[1:(cocap+1),1:(cocap+1)]
  
  
  # Find the starting row that will not display 
  for(i in 1:(cocap+1)) {
    if(any(xx[i,]=="D" | xx[i,]=="S" | xx[i,]=='E')==FALSE) {
      cut <- i+1
      break
    } 
  }
  xxnew <- xx[-(cut:(cocap+1)),]
  
  xxnew_reform <- xxnew %>% tidyr::pivot_longer(cols = 2:ncol(xxnew), values_to = "decision", names_to = "nsubj") %>% 
    dplyr::filter(decision != ".") %>%
    dplyr::mutate(nsubj = as.numeric(unlist(stringr::str_match_all(nsubj, "[0-9]+"))), 
                  ndlt = as.factor(No.of.DLTs)) 
  
  xxnew_reform$nsubj <- factor(xxnew_reform$nsubj, levels = sort(unique(xxnew_reform$nsubj)))
  
  
  ## generate the decision table
  fig <- ggplot2::ggplot(data = xxnew_reform, ggplot2::aes(x = nsubj, y = ndlt, fill = decision)) + 
    ggplot2::geom_tile(color = "white") + 
    ggplot2::scale_y_discrete(limits = rev(levels(xxnew_reform$ndlt))) + 
    ggplot2::scale_x_discrete(position = "top") + 
    ggplot2::scale_fill_manual(values = c("#0063c3", "#d34d2f",  "#88c765", "#ec951a"), 
                               labels = c("D", "DU", "E", "S")) + 
    ggplot2::theme(legend.position = "bottom", 
                   axis.text = ggplot2::element_text(face = "bold", size = 15), 
                   axis.title = ggplot2::element_text(face = "bold", size = 20), 
                   plot.title = ggplot2::element_text(face = "bold", size = 25)
                   ) + 
    ggplot2::labs( x = "Number of Subjects at Current Dose", y = "Number of DLTs")
  
  return(list(decision_fig = fig, decision_table = xx))
  
}



#' Given number of subjects and DLTs, estimate the MTD by isotonic regression
#' and empirical estimation
#'
#' @param cohort_size a vector of number of subjects in each cohort
#' @param n_dlt a vector of number of DLTs (must have the same length as
#'   \code{cohort_size})
#' @param target the target toxicity 
#'
#' @return a table of results for the estimates
#' @export
#'
#' @examples
#'  n_dlt <- c(0,1,0,0,0,2); cohort_size <- c(3,6,3,6,3,6)
#' estimate_dlt_isoreg(cohort_size, n_dlt, target = 0.3)
estimate_dlt_isoreg <- function(cohort_size, n_dlt, target, method = "selfCoded"){
  
  
  y <- cohort_size; x <- n_dlt; target_tox <- target;
  use_y <- which(y != 0) # which cohort has enrolled patients
  doses <- 1:length(y); 
  est_raw <- est_iso <- mtd <- rep(NA, length(y))
  
  if (method == "statsPackage"){
    tox_isoreg <- stats::isoreg(x[use_y]/y[use_y])$yf
  } else if(method == "selfCoded"){
    tox_isoreg <- isotonic_reg(x = x[use_y], y = y[use_y])
  }
  
  diff_target <- tox_isoreg - target_tox
  target_dose <- which(abs(diff_target) == min(abs(diff_target)))
  if(length(target_dose) == 1){
    result <- target_dose
  } else {
    diff1 <- diff_target[target_dose]
    if (min(diff1) >= 0 ){ # get the smallest if > target toxicity
      result <- head(target_dose, 1)
    } else{ # if equidistance from left or right, pick the max dose from the left 
      result <- tail(target_dose[which(diff1 < 0)], 1)
    }
  }
  final_result <- use_y[result] # the dose sequence for MTD  
  est_raw[use_y] <- x[use_y]/y[use_y]
  est_iso[use_y] <- tox_isoreg
  mtd[final_result] <- "MTD"
  
  indat <- tibble::tibble(doses = doses, n = cohort_size, dlts = n_dlt, 
                          est_raw = est_raw, est_iso = est_iso, mtd = mtd)
  
  return(indat)
}


#' isotonic regression for estimating probability of MTD
#'
#' @param x vector of number of toxicities
#' @param y vector of number of subject at this dose level
#'
#' @return a vector of toxicity probabilities
#' @export
#' @keywords internal
# #' @examples
isotonic_reg <- function(x, y){
  
  if(length(x) != length(y)){
    stop("vector lengths do not match")
  }
  ndose <- length(x)
  p <- rep(NA, length(x))
  i <- 1
  while(i <= ndose){
    p1 <- cumsum(x[i:ndose])/cumsum(y[i:ndose])
    min_index <- which.min(p1) + sum(!is.na(p))
    p[i:min_index] <- min(p1)
    i <- min_index + 1
  }
  return(p)
}



