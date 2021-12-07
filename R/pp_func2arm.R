#' Calculate predicative probability density
#'
#' @param x number of respond
#' @param n number of subjects already enrolled
#' @param nmax max number of subjects to be enrolled
#' @param p0 hypothesized response rate
#' @param a,b beta priors
#'
#' @return a tibble 
#' @export
#'
#' @examples
#' predp(x = 3, n = 10, nmax = 30, p0 = 0.3, a = 1, b = 1)
predp <- function(x, n, nmax, p0, a, b){
  p1 <- rep(NA, nmax - n + 1)
  p2 <- p1
  
  for(i in 0:(nmax -n)){
    ###beta-binomial prob mass func p(Y=i|x)
    p1[i + 1] <- choose(nmax - n, i)*beta(i + a + x, nmax - i + b - x)/
      beta(a + x, b + n - x)
    ### posterior prob given X=x and Y=i p(P>p0|X=x, Y=i) (i.e. B_i in Lee and Liu)
    p2[i + 1] <- stats::pbeta(p0, i + a + x, b + nmax - x - i, lower.tail = FALSE)
  }
  
  p <- tibble::tibble(Y=c(0:(nmax-n)),p1, p2)
  return(p)
}


#' Predicative probabilities and decision tables
#'
#' @param nmax max sample size
#' @param nmin minimum sample size for the first interim 
#' @param p0 lower reference value of response rate 
#' @param p1 target value of response rate
#' @param thetats posterior probability for Final Go s.t. Prob(p > p0) > thetats
#' @param thetatf posterior probability for final NoGo Prob(p > p0) <= thetatf
#' @param thetau Predictive probability for Go, i.e., PredP(Go) > thetau
#' @param thetal1 Predictive probability for NoGo PredP(NoGo) > thetal1
#' @param a,b priors for beta distribution 
#'
#' @return a decision table 
#' @export
#' 
#' @examples
#' go_nogo <- bayes_pred_go_nogo(nmax = 40, nmin = 1, p0 = 0.3, p1 = 0.5,
#' thetats = 0.8, thetatf = 0.1, thetau = 0.95, thetal1 = 0.95, a = 1, b = 1)
#' 
bayes_pred_go_nogo = function(nmax = 40, nmin = 1, p0 = 0.3, p1 = 0.5, thetats = 0.8, 
                 thetatf = 0.1, thetau = 0.95, thetal1 = 0.95, a = 1, b = 1){
  
  # check parameter requirement
  if(nmax < nmin){
    stop("nmax should be no less than nmin")
  } 
  if(p0*(p0-1) > 0){
    stop("p0 should be between (0, 1)")
  }
  if(p1*(p1-1) > 0){
    stop("p1 should be between (0, 1)")
  }
  if(thetats*(thetats-1) > 0){
    stop("thetats should be between (0, 1)")
  }
  if(thetatf*(thetatf-1) > 0){
    stop("thetatf should be between (0, 1)")
  }
  if(thetau*(thetau-1) > 0){
    stop("thetau should be between (0, 1)")
  }
  if(thetal1*(thetal1-1) > 0){
    stop("thetal1 should be between (0, 1)")
  }
  
  eps <- 1e-7
  # initiate the decision matrix
  gngmat <- matrix(rep(NA, (nmax - nmin + 1)*(nmax + 1)), nrow = (nmax - nmin + 1))
  rownames(gngmat) <- nmin:nmax
  colnames(gngmat) <- 0:nmax
  
  for(n in nmin:nmax){
    for(x in 0:n){
      ##prob of P(Y=i|x) and Post.p(P>p0|X, Y=i)
      prob <- predp(x = x, n = n, nmax = nmax, p0 = p0, a = a, b = b)    
      ####prob of P(Y=i|x) and Post.p(P>p1|X, Y=i)
      probt <- predp(x = x, n = n, nmax = nmax, p0 = p1, a = a, b = b)    
      ## indicate whether the B_i > thetats (see Lee and Liu)  (for go)
      ivec1 <- prob$p2 > thetats                 
      ## Predictive probability
      pp1 <- sum(prob$p1[ivec1])             
      ## indicate whether the B_i (using P1) < thetatf (for no go)
      ivec2 <- probt$p2 <= thetatf                
      pp2 <- sum(probt$p1[ivec2])
      
      if (pp1 - thetau > eps) {gngmat[n - nmin + 1, x + 1] <- 1}
      if (pp2 - thetal1 > eps) {gngmat[n - nmin + 1, x + 1] <- 0}
    }
  }
  return(gngmat)
  
}


#' Plot the decision matrix
#'
#' @param pptabx the result returned by \code{\link{bayes_pred_go_nogo}}
#' @param groupsize additional number of subjects needed for each interim after
#'   the first interim
#'
#' @return a plot of decision matrix (ggplot object)
#' @export 
#' @import dplyr ggplot2
#' @examples
#' go_nogo <- bayes_pred_go_nogo(nmax = 40, nmin = 1, p0 = 0.3, p1 = 0.5,
#' thetats = 0.8, thetatf = 0.1, thetau = 0.95, thetal1 = 0.95, a = 1, b = 1)
#' gngplot(go_nogo, groupsize = 5)
gngplot <- function(pptabx, groupsize){
  
  outcome <- nresp <- nsbj <- NULL 
  nr <- nrow(pptabx)
  rowseq <- unique(c(seq(1, nr, groupsize), nr))
  colcut <- match(1, pptabx[nr, ])+1    ##determine how many columns to show
  if (is.na(colcut)) {colcut <- match(NA, pptabx[nr, ]) + 1}
  pptab <- pptabx[rowseq, 1:colcut]
  nr <- nrow(pptab)
  nc <- ncol(pptab)
  rnames <- rownames(pptab)
  cnames <- colnames(pptab)
  pptab <- data.frame(pptab); names(pptab) <- cnames
  
      # pptab1 <- tidyr::pivot_longer(pptab, cols = 1:ncol(pptab),
      #                     values_to = "outcome", names_to = "nresp") 
  pptab1 <- tibble::tibble(nresp = rep(cnames, length(rnames)), 
                           outcome = as.vector(t(pptab)))  
  pptab1 <- pptab1 %>% dplyr::mutate(nsbj = rep(rnames, each = length(cnames))) %>%
    dplyr::mutate(outcome = ifelse(is.na(outcome), 0.5, outcome)) %>% 
    dplyr::mutate(nresp = as.numeric(nresp), 
                  nsbj = as.numeric(nsbj)) %>% 
    dplyr::filter(nresp <= nsbj)
  
  pptab1 <- pptab1 %>% dplyr::mutate(
    label_val = paste0(round(nresp/nsbj*100, 1), "%"),
    outcome = factor(outcome, levels = c(0, 0.5, 1)), 
    nsbj = factor(nsbj, levels = sort(unique(pptab1$nsbj))),
    nresp = factor(nresp, levels = sort(unique(pptab1$nresp)))
  ) %>% 
    dplyr::group_by(nsbj, outcome) # %>%
    # dplyr::mutate(label_val = dplyr::case_when(
    #   as.numeric(outcome) == 1 & dplyr::row_number() == dplyr::n() ~ label_val, 
    #   as.numeric(outcome) == 3 & dplyr::row_number() == 1 ~ label_val, 
    #   TRUE ~ ""
    # ))
    # 
  
  res1 <- ggplot(data = pptab1, aes(x = nresp, y = nsbj, fill = outcome)) + 
    geom_tile(color = "white") + 
    #theme_bw() +
    scale_y_discrete(limits = rev(levels(pptab1$nsbj))) + 
    scale_fill_manual(values = c("red","darkgrey", "darkgreen"), 
                      labels = c("No Go", "Continue", "Go") ) + 
    theme(legend.position = "bottom", 
          axis.text = element_text(face = "bold", size = 15), 
          axis.title = element_text(face = "bold", size = 20), 
          plot.title = element_text(face = "bold", size = 25)) + 
    labs(x = "Number of Responses", y = "Number of Subjects", 
         title = "Decision Matrix") +
    geom_text(label = pptab1$label_val)
  
  return(res1)
}

