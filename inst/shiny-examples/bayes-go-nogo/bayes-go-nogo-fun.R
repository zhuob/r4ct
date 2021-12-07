
#' Simulation function
#'
#' @param orr response rate for data to be simulated from, can be a vector
#' @param nmin minimum sample size to perform interim analysis
#' @param nmax maximum number of subjects to be enrolled
#' @param nsim number of simulations 
#' @param pptab output from function \code{\link{bayes_pred_go_nogo}}, the
#'   decision rule
#' @param groupsize number of additional subjects for each interim look
#'
#' @return
#' @export
#'
#' @examples
predsim = function(orr, nmin, nmax, nsim, pptab, groupsize = 1){
  
  npar <- length(orr)
  
  for (i_npar in 1:npar) {
    ###total samples to generate
    nsam <- nsim * nmax   
    ### generate binary samples
    samp <- rbinom(nsam, 1, orr[i_npar])   
    ### for a matrix, each row being a trial
    sampm <- matrix(samp, nrow = nsim)     
    ### number of rows of the decision table
    nr <- nrow(pptab)                    
    
    ##determine rows position (row #) for each interim analysis, 
    # "nr", the final row # is added regardless of the groupsize
    rowseq <- unique(c(seq(1, nr, groupsize), nr))  
    ## the rownames are the sample size from nmin to nmax, 
    rnm <- as.numeric(rownames(pptab)[rowseq])   
    
    ##here we only select the sample sizes we will do interim  
    ### number of interim (including the final)
    nlooks <- length(rnm)                
    ## store the go/nogo decision at each monitoring
    gngm <- matrix(rep(-1, nsim*nlooks), nrow = nsim)    
    ##  use sample size at interim as the column names.
    colnames(gngm) <- paste(rnm)  
    ### summary of go/nogo decision at each monitoring
    gngmsum <- matrix(rep(-1, nlooks*3), nrow = nlooks)    
    gngmsumx <- matrix(rep(-1, nlooks*6), nrow = nlooks)
    
    for (i in 1:nlooks) {
      # j is the number of subjects at interim look, 
      # i is the ith interim look
      j <- rnm[i]               
      # k is the row number in pptab at which interim will be performed
      k <- rowseq[i]            
      # summarize # of responses at j subjects
      sumj <- c(apply(sampm[,1:j], 1, sum))      
      # apply the go/nogo criteria at j subjects (as a column in gngm matrix)
      gngm[, i] <- pptab[k, sumj + 1]            
      gngmsum[i,]<- c(sum(gngm[,i]==1, na.rm = T)/nsim,
                      sum(is.na(gngm[,i]))/nsim,
                      sum(gngm[,i]==0, na.rm = T)/nsim) 
    }
    endpred <- gngm[,nlooks]
    gngmx <- gngm
    
    for (i in 1:(nlooks-1)) {     
      ###This loop can be rewritten using cumulative sum instead of carry over
      ###to the next cells
      
      ### flag the non NA cells (i.e. cells with decisions made for an interim
      # analysis across nsim trials)
      flg <- !is.na(gngmx[,i])        
      ### carry the decision over until the end via looping.
      gngmx[,i+1][flg] <- gngmx[,i][flg]      
      ### flag to indicate whether a prediction is made (Go or Nogo, 
      # nondecision is not counted)
      gngflg <- !is.na(gngm[,i])            
      accord <- (gngm[,i][gngflg]==endpred[gngflg])   
      # In those predictions made, "accord" indicate whether the 
      # prediction agrees with the final decision
      accord <- ifelse(is.na(accord), F, accord)
      ## calculate the prob of go or nogo
      gngmsumx[i,] <- c(rnm[i], round(sum(gngmx[,i]==1, na.rm=T)/nsim,3),
                        round(sum(is.na(gngmx[,i]))/nsim,3), 
                        round(sum(gngmx[,i]==0, na.rm=T)/nsim,3), 
                        sum(gngflg)/nsim, round(sum(accord)/sum(gngflg),3))  
      
    }
    #### this flag to indicate whether any decision was made cumulatively up to
    # the last interim before final
    gngflg <- !is.na(gngmx[,nlooks-1])      
    accord <- (gngmx[,nlooks-1][gngflg] == endpred[gngflg])
    ###same as above, but for the last final cumulative analysis
    accord = ifelse(is.na(accord), F, accord)
    gngmsumx[nlooks,] <- c(rnm[nlooks], round(sum(gngmx[,nlooks]==1, na.rm=T)/nsim,3),
                           round(sum(is.na(gngmx[,nlooks]))/nsim,3), 
                           round(sum(gngmx[,nlooks]==0, na.rm=T)/nsim,3), 
                           sum(gngflg)/nsim, round(sum(accord)/sum(gngflg),3) )   
    ### average sample size assuming stopping trial after go or nogo decisions
    avgsmp = avessize(gngmsumx)   
    ### organize the results into a frame
    gngmsumy = cbind(i_npar,orr[i_npar],gngmsumx, avgsmp)    
    
    #### stack the frame.
    if (i_npar==1) {gngmsum3 = gngmsumy} else {
      gngmsum3=rbind(gngmsum3, gngmsumy)}     
  }
  
  colnames(gngmsum3)<- c("Case", "True_P", "N","GO_P", "Cont_P", "No_Go_P",
                         "Pct_Pred", "Pred_Accuracy", "Ave_SS" )  
  return(gngmsum3)
  
}




#' Plot Go/NoGo probability by interim
#'
#' @param psimx an object returned by \code{\link{predsim}}
#' @param p0 lower reference value 
#' @param ptv 
#' @param casen 
#' @param lgdpos 
#'
#' @return
#' @export
#'
#' @examples
plotsim <- function(psimx, p0, ptv,casen, lgdpos = 1){
  
  psim1 <- data.frame(psimx)
  casemax <- max(psim1$Case)
  if (casen > casemax) {casen = casemax}
  psim2 <- filter(psim1, Case == casen)
  p1 <- psim2$True_P[1]
  psim <- select(psim2, -c(1,2))
  nr <- nrow(psim)
  nmax <- max(psim$N)
  nmin <- min(psim$N)
  seq1 <- psim$N
  rnames <- paste(psim$N)
  
  psim3 <- tidyr::gather(data = psim, key = "Interim Decision", 
                         value = "prob", -c(1, 5:ncol(psim))) %>% 
    dplyr::mutate(`Interim Decision` = dplyr::case_when(
      `Interim Decision` == "GO_P" ~ "Go", 
      `Interim Decision` == "Cont_P" ~ "Continue", 
      `Interim Decision` == "No_Go_P" ~ "No Go"
    )) %>% 
    dplyr::mutate(`Interim Decision` = factor(`Interim Decision`, 
                                              levels = c("No Go", "Continue", "Go")))
  
  title_name <- paste0("LRV = ", p0, ", TV = ", ptv, ", True p = ", p1)
  
  res1 <- ggplot(data = psim3, aes(x = N, y = prob, group = `Interim Decision`)) + 
    geom_line(aes(color = `Interim Decision` ), size = 1.5) + 
    geom_point(aes(color = `Interim Decision` ), size = 4) + 
    ylim(c(0, 1)) +
    theme(legend.position = "bottom",
          axis.text = element_text(face = "bold", size = 10), 
          axis.title = element_text(face = "bold", size = 12), 
          plot.title = element_text(face = "bold", size = 15)) + 
    labs(x = "Number of Subjects at Interim", y = "Probability", 
         title = title_name) + 
    geom_text(label = psim3$prob, label.size = 0.5) + 
    scale_color_manual(values = c("red", "darkgrey", "darkgreen"))
  
  return(res1)
  
}


plotpred=function(psimx, p0, casen, lgdpos=1){
  psim1=data.frame(psimx)
  casemax=max(psim1$Case)
  if (casen>casemax) {casen=casemax}
  psim2=filter(psim1, Case==casen)
  p1=psim2$True_P[1]
  psim=select(psim2, -c(1,2))
  nr=nrow(psim)
  nmax=max(psim$N)
  nmin=min(psim$N)
  seq1=psim$N
  rnames=paste(psim$N)
  avess = avessize(psim)
  plot(c(nmin-0.2, nmax+1), c(-0.1, 1.1), type="n", axes=F, xlab="", ylab="")
  lines(seq1[1:(length(seq1)-1)], psim$Pct_Pred[1:(length(seq1)-1)], lty=1, col="blue", lwd=3)
  points(seq1[1:(length(seq1)-1)], psim$Pct_Pred[1:(length(seq1)-1)], pch=1, col="blue", lwd=3)
  lines(seq1[1:(length(seq1)-1)], psim$Pred_Accuracy[1:(length(seq1)-1)], lty=1, col="red", lwd=3)
  points(seq1[1:(length(seq1)-1)], psim$Pred_Accuracy[1:(length(seq1)-1)], pch=1, col="red", lwd=3)
  points(seq1[length(seq1)],psim$Pct_Pred[length(seq1)], pch=17, col="blue",lwd=7)
  points(seq1[length(seq1)],psim$Pred_Accuracy[length(seq1)], pch=17, col="red",lwd=7  )
  
  
  abline(v=nmin, lty=2, col="gray")
  abline(h=seq(0, 1, 0.2), lty=2, col="gray")
  text(seq1[1:(length(seq1)-1)], rep(-0.1, length(seq1)-1),rnames[1:(length(seq1)-1)])
  text(seq1[length(seq1)],-0.1, "Overall")
  text(rep(nmin-0.8, 11),seq(0,1,0.1),seq(0,100,10), cex=0.8)
  title(ylab="Percent(%)", line=2, cex.lab=1)
  title(xlab="Number of Patients at Interim", line=1.5, cex.lab=1)
  legend(nmin,lgdpos, c("% Making Pred.", "Pred.Accuracy(%)"), pch=c(1,1), lwd=3, col=c("blue", "red"), lty=c(1,1), bty="n", horiz=T)
  text(nmin+(nmax-nmin)*0.5, 1,  paste("Target p=", p0, ", True p=", p1, ", Average Sample Size=", avess, sep=""), pos=3, cex=0.8)
} 










###Calculate the expected sample size based on the go nogo probability###
avessize = function(psim){
  nr=nrow(psim)
  ss=psim[1,1]*(psim[1,2]+psim[1,4])
  ##    ss=psim$N[1]*(psim$GO_C[1]+psim$NoGo_C[1])
  for (i in 2:nr) {
    ss=ss+psim[i,1]*((psim[i,2]+psim[i,4])-(psim[i-1,2]+psim[i-1,4]))
    ##      ss=ss+psim$N[i]*((psim$GO_C[i]+psim$NoGo_C[i])-(psim$GO_C[i-1]+psim$NoGo_C[i-1]))
  }
  ss=ss+psim[nr,1]*(1-psim[nr,2]-psim[nr,4])
  ##    ss=ss+psim$N[nr]*(1-psim$GO_C[nr]-psim$NoGo_C[nr])
  tt=round(ss,1)
  return(tt)
}




tabsim = function(orr, nmin, nmax, nsim, pptab, groupsize=1){     ### orr can be an array
  ### groupsize: number of extra patients for each interim look
  ### nmax        ### maximum number of patients to be enrolled
  ### nmin        ###first time to do the interim monitoring
  ### pptab: outpur from function bayes_pred_go_nogo(), is the decision rules
  npar=length(orr)
  
  for (i_npar in 1:npar) {
    
    nsam = nsim*nmax   ###total samples to generate
    samp=rbinom(nsam, 1, orr[i_npar])   ### generate binary samples
    sampm = matrix(samp, nrow=nsim)     ### for a matrix, each row being a trial
    nr=nrow(pptab)                    ### number of rows of the decision table
    rowseq=unique(c(seq(1,nr, groupsize), nr))  ##determine rows position (row #) for each interim analysis, "nr", the final row # is added regardless of the groupsize
    rnm = as.numeric(rownames(pptab)[rowseq])   ##the rownames are the sample size from nmin to nmax, 
    ##here we only select the sample sizes we will do interim  
    nlooks = length(rnm)                ### number of interim (including the final)
    gngm = matrix(rep(-1, nsim*nlooks), nrow=nsim)    ## store the go/nogo decision at each monitoring
    gngmx=gngm
    colnames(gngm)=paste(rnm)  ##  use sample size at interim as the columne names.
    gngmsum = matrix(rep(-1, nlooks*3), nrow=nlooks)    ### summary of go/nogo decision at each monitoring
    gngmsumx=matrix(rep(-1, nlooks*4), nrow=nlooks)
    
    for (i in 1:nlooks) {
      j=rnm[i]               #### j is the number of subjects at interim look, i is the ith interim look
      k=rowseq[i]            #### k is the row number in pptab at which interim will be performed
      sumj = c(apply(sampm[,1:j], 1, sum))      # summaize # of responses at j subjects
      gngm[, i] = pptab[k, sumj+1]            # apply the go/nogo criteria at j subjects (as a column in gngm matrix)
      gngm[, i] = ifelse(is.na(gngm[, i]), -1, gngm[, i])   ### use -1 to replace NA for easy computing in this case
      if (i==1) {gngmx[,i]=gngm[,i]} else {xx=gngmx[, i-1]; gngmx[,i]=ifelse (xx == -1, gngm[,i], xx) }
      gngmsum[i,]=c(sum(gngm[,i]==1)/nsim,sum(gngm[,i]==-1)/nsim, sum(gngm[,i]==0)/nsim) 
      gngmsumx[i,]=c(rnm[i], sum(gngmx[,i]==1)/nsim, sum(gngmx[,i]==-1)/nsim, sum(gngmx[,i]==0)/nsim)
    }
    
    endpred=gngm[,nlooks]          ####final action at end of cohort
    predx = gngmx[, nlooks]       #### cumulative predicted results (including final)
    
    accord = sum(endpred==predx)/nsim   ##Accordance rate between continuous monitoring and the single final decision
    if (nlooks>2) {
      early = sum(apply(gngmx[,1:(nlooks-1)],1,max)>=0)/nsim   ## 1. subset, excluding the last column. 2.get the max value of each row. 3. max>=0 mean a go or no-go decision is made. 4. add up all decisions numbers divided by the number of simulations
    } else {early = sum(gngmx[,1:(nlooks-1)]>=0)/nsim}
    
    ngoprob= sum(gngmx[,nlooks]==0)/nsim; undprob=sum(gngmx[,nlooks]==-1)/nsim; goprob = sum(gngmx[,nlooks]==1)/nsim    ### cumulative go/nogo proportion (including final), did not use table statement for robustness
    
    loc_go = data.frame(which(gngmx==1, arr.ind = TRUE))   ### find the locations (row, col) of go decision in the matrix
    loc_go_a = arrange(loc_go, row)               ###sort by rows
    loc_go_b = loc_go_a[!duplicated(loc_go_a$row),]  ###remove the duplicated rows, only keep the first row, that is, only the first go decision counts
    samgo = round(mean(rnm[loc_go_b$col]), 1)     ### for the trials with the go decisions, collect the sample sizes, and take average
    
    loc_ngo = data.frame(which(gngmx==0, arr.ind = TRUE))
    loc_ngo_a = arrange(loc_ngo, row)               ###sort by rows
    loc_ngo_b = loc_ngo_a[!duplicated(loc_ngo_a$row),]  ###remove the duplicated rows, only keep the first row, that is, only the first go decision counts
    samngo = round(mean(rnm[loc_ngo_b$col]),1)     ### for the trials with the go decisions, collect the sample sizes, and take average
    samund = max(rnm)    ## if undecided until the end, the sample size will be the max
    
    gngmsumz = data.frame(c("Go", "No Go", "Undecided"), c(goprob, ngoprob, undprob), c(samgo, samngo, samund), 100*early, 100*accord,  row.names=NULL)
    
    gngmsumy = cbind(i_npar,orr[i_npar],gngmsumz)    ### organize the results into a frame
    
    if (i_npar==1) {gngmsum3 = gngmsumy} else {gngmsum3=rbind(gngmsum3, gngmsumy)}     #### stack the frame.
  }
  
  colnames(gngmsum3)=c("Case", "True_P", "Decision", "GNG_Prob", "AVE_SSIZE", "% Early Decision", "% Concordance" )  
  return(gngmsum3)
}



tabsim_new = function(orr, nmin, nmax, nsim, pptab, groupsize=1){     ### orr can be an array
  ### groupsize: number of extra patients for each interim look
  ### nmax        ### maximum number of patients to be enrolled
  ### nmin        ###first time to do the interim monitoring
  ### pptab: outpur from function bayes_pred_go_nogo(), is the decision rules
  npar=length(orr)
  
  temp <- list()
  for (i_npar in 1:npar) {
    
    sim_decision <- sim_table(nmin = nmin, nmax = nmax, orr = orr[i_npar], nsim = nsim, 
                              decision_matrix = pptab, groupsize = groupsize) 
    
    trans_decision <- process_decision(sim_decision) 
    temp[[i_npar]] <- summary_decision(trans_decision, nmax = nmax) %>% 
      mutate(case = orr[i_npar]) %>% 
      select(case, everything())
    
  }
  result <- dplyr::bind_rows(temp)
}  





make_decision_by_look <- function(resp_matrix, look, decision_matrix){
  
  # get the decision space from the decision matrix
  decision <- matrix(NaN, ncol= length(look), nrow = nrow(resp_matrix))
  
  for(i in 1:length(look)){
    # number of responder at look
    n_resp <- apply(resp_matrix[, 1:look[i]], 1, sum)
    decision_row <- decision_matrix[as.numeric(rownames(decision_matrix)) == look[i]] 
    decision_space <- decision_row[1:(look[i] + 1)]
    # make the decision; remember decision_space[1] corresponds to 0 resp, therefore + 1
    decision[, i] <- decision_space[n_resp + 1]
    
  }
  
  return(decision)
}


#' Simulate binary response and return the decision results by comparing to
#' decision matrix.
#'
#' @param nmin the minimun sample size to look at
#' @param nmax the maximum sample size 
#' @param orr the objective response rate
#' @param nsim number of simulations to be run
#' @param decision_matrix the decision matrix
#' @param groupsize what is the subject number enrolled after each interim
#'
#' @return a matirix with rows representing each simulation, and columns
#'   decision made at each interim. \code{1 = GO},  \code{0 = NoGo},  \code{NaN
#'   = undecided}
#' @export
#'
#' @examples
sim_table <- function(nmin, nmax, orr, nsim, decision_matrix, groupsize = 1){
  
  # simulate response
  resp <- rbinom(nmax * nsim, 1, orr)
  # initiate response matrix, so each row is a simulated response vector
  sim_resp <- matrix(resp, nrow = nsim, ncol = nmax)
  
  look <- unique(c(seq(nmin, nmax, groupsize), nmax))
  decision <- tibble::as_tibble(matrix(NA, ncol = length(look), nrow = nsim)) 
  names(decision) <- look
  decision[, 1:ncol(decision)] <- make_decision_by_look(resp_matrix = sim_resp, 
                                                        look = look, decision_matrix)
  
  return(decision)
}


#' process the simulated decision data to derive quantities for summary table
#'
#' @param sim_decision
#'
#' @return the same data with four additional columns 
#' \itemize{
#'   \item{decision_first_avail }{the first GO noGO decision in a decision
#'   sequence} 
#'   \item{decision_prior_final }{the decision at last interim look,
#'   this is the collective decision made prior to final} 
#'   \item{decision_final       }{the final decision}
#'   \item{obs_at_first_decision}{when the first decision is made, what is the
#'   sample size}
#'   }
#' @export
#'
#' @examples
process_decision <- function(sim_decision){
  
  # get sample size at each look
  look_nsbj <- as.numeric(names(sim_decision))
  # get number of simulations
  nsim <- nrow(sim_decision)
  
  ## find the last interim decision, so (NA, 1, NA, NA, 0, NA) will be
  ## updated as (NA, 1, 1, 1, 0, 0), since if 1 (success) is claimed at look 2, 
  ## then at look 3 and 4, they should also be success
  
  decision <- as.matrix(sim_decision)
  for (i in 1:length(look_nsbj)){
    if(i == 1){decision[, i] <- decision[, i]} 
    else{ 
      temp <- decision[, i]
      decision[, i] <- ifelse(is.na(temp), decision[, i-1], temp)
    }
  }
  
  # the final decisoin by simulation
  decision_final <- decision[, length(look_nsbj)]
  
  # the decision prior to final 
  
  decision_prior_final <- decision[, length(look_nsbj)-1]
  
  decision_first_locate <- which(!is.na(sim_decision), arr.ind = TRUE) %>% 
    as_tibble() %>% arrange(row, col) %>% group_by(row) %>%
    filter(row_number() == 1) %>% as.matrix()
  decision_first_location_na <- setdiff(1:nsim, decision_first_locate[, 1])
  decision_first_locate <- rbind(decision_first_locate, cbind(decision_first_location_na, 
                                                              rep(length(look_nsbj), length(decision_first_location_na)))) 
  decision_first_locate <- decision_first_locate[order(decision_first_locate[, 1]),]
  decision_first_avail <- decision[decision_first_locate]
  obs_at_first_decision <- look_nsbj[decision_first_locate[, 2]]
  
  
  proc_decision <- tibble::as_tibble(sim_decision)
  names(proc_decision) <- paste0("look", look_nsbj)
  proc_decision <- proc_decision %>% 
    mutate(decision_first_avail = decision_first_avail, 
           decision_prior_final = decision_prior_final, 
           decision_final = decision_final,
           obs_at_first_decision = obs_at_first_decision)
  
  return(proc_decision)
  
}


#' Title
#'
#' @param trans_decision the data returned by \code{\link{process_decision}} 
#'
#' @return a data frame with corresponding key summary statistics
#' @export
#'
#' @examples
summary_decision <- function(trans_decision, nmax = 40){
  
  
  nsim <- nrow(trans_decision)
  decision_final <- trans_decision %>% pull(decision_final)
  decision_first_avail <- trans_decision %>% pull(decision_first_avail)
  decision_prior_final <- trans_decision %>% pull(decision_prior_final) 
  obs_at_first_decision <- trans_decision %>% pull(obs_at_first_decision)
  # sample size
  # s_size <- obs_at_first_decision[which(decision_first_avail == 1)]
  s_size <- ifelse(decision_first_avail == 1 & !is.na(decision_first_avail), obs_at_first_decision, nmax)
  en_s <- ifelse(length(s_size) == 0, 0, mean(s_size))
  f_size <- ifelse(decision_first_avail == 0 & !is.na(decision_first_avail), obs_at_first_decision, nmax)
  # f_size <- obs_at_first_decision[which(decision_first_avail == 0)]
  en_f <- ifelse(length(f_size) == 0, 0, mean(f_size))
  en <- mean(obs_at_first_decision)
  
  es_prior_to_final <- sum(decision_prior_final == 1, na.rm = TRUE)/nsim
  ef_prior_to_final <- sum(decision_prior_final == 0, na.rm = TRUE)/nsim
  early_decision <- sum(!is.na(decision_prior_final))/nsim
  # decision is based on the first available decision
  efficacy <- sum(decision_first_avail == 1, na.rm = TRUE)/nsim
  futility <- sum(decision_first_avail == 0, na.rm = TRUE)/nsim
  # percentage of confirmatory decisions
  s_confirm <- sum(decision_final == decision_first_avail
                   & decision_final == 1, na.rm = TRUE) 
  es_confirm <- s_confirm/sum(decision_first_avail == 1, na.rm = TRUE)
  f_confirm <- sum(decision_final == decision_first_avail
                   & decision_final == 0, na.rm = TRUE) 
  ef_confirm <- f_confirm/sum(decision_first_avail == 0, na.rm = TRUE)
  
  final <- tibble::tibble(efficacy, es_confirm, futility, ef_confirm, en, 
                          early_decision, en_s, es_prior_to_final, en_f, ef_prior_to_final)
  return(final)
  
}

