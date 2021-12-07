

# Function to perform simulations
#' Run simu
#'
#' @param B 
#' @param scen 
#' @param cohs 
#' @param ncohd 
#' @param ncoh 
#' @param mxn 
#' @param pt 
#' @param pt.a 
#' @param pt.b 
#' @param tdose 
#' @param nds 
#' @param ncoh1 
#' @param nds1 
#' @param nds2 
#' @param Escf 
#' @param apr 
#' @param bpr 
#' @param p.ud 
#' @param dslv_start 
#'
# #' @return
#' @keywords internal
# #' @export
#'
# #' @examples
mTPIeval=function(B=B0,scen=1,cohs=cohs0,ncohd=ncohd0,
                  ncoh=ncoh,mxn=mxn,pt=pt,pt.a=pt1,pt.b=pt2,tdose,nds=nds,ncoh1=ncoh1,nds1=nds1,
                  nds2=nds2,Escf=Escf,apr=apr,bpr=bpr,p.ud=p.ud, 
                  dslv_start = 5)	
{
  
  
#   mTPIeval: no visible binding for global variable 'B0'
  B0 <- cohs0 <- ncohd0 <- pt1 <- pt2 <- NULL
  
  trsk=tdose; trsk2=c(0,trsk,1)				## True toxicity by doses
  hzr =-log(1-trsk2); hzd=diff(c(0,hzr))				## hazard increments by doses
  Rest33 =array(0,c(B,5));	Resd33=array(0,c(B,nds,2))
  RestTPI=array(0,c(B,6));	ResdTPI=array(0,c(B,nds,3))
  
  ttrch=0;rchcap=0;# to calcuate the %of cap
  #    set.seed(12345)
  for (ib in 1:B)	{
  #  set.seed(ib)
    # print(ib)
    dlti=apply(matrix(stats::rbinom(cohs*ncoh1*nds2,1,1-exp(-hzd)),nds2,cohs*ncoh1),2,cumsum)
    dlti[dlti>=1]=1						## DLT by subjects w/ dummy doses(2) & cohort(1)
    dltc=apply(array(dlti,c(nds2,cohs,ncoh1)),c(1,3),sum)	## DLT summary by risks and cohorts
    ## 3+3
    sds=matrix(0,nds2,5); sds[nds2,3]=3				## Escalation summary by doses
    colnames(sds)=c("dlts","n","esc","MTD","")
    icoh=0; dslv=1; 
    nchi=0							## cohort i, dose i, cohorts at dose i
    while (icoh < ncoh1)	{
      icoh=icoh+1
      sds[dslv,1]=sds[dslv,1]+dltc[dslv,icoh]
      sds[dslv,2]=sds[dslv,2]+cohs
      if (sds[dslv,2]==cohs)		{
        if (sds[dslv,1]==0)	{sds[dslv,3]=1; dslv=dslv+1}	
        else	{
          if (sds[dslv,1]==1)	{sds[dslv,3]=2	}
          if (sds[dslv,1]>=2)	{ sds[dslv,3]=3;
          if (sds[dslv-1,2]==cohs) {dslv=dslv-1}	
          else	{
            if (sds[dslv-1,2]==2*cohs) {sds[dslv-1,4]=dslv-1; icoh=ncoh1}
          }	
          }	
        }
      }
      if (sds[dslv,2]==2*cohs)	{
        if (sds[dslv,1]<=1){ sds[dslv,3]=1;
        if (sds[dslv+1,3]==3) {sds[dslv,4]=dslv; icoh=ncoh1} 
        if (sds[dslv+1,3] <3) dslv=dslv+1
        }		
        else	{
          if (sds[dslv,1]>=2)	{ sds[dslv,3]=3;
          if (sds[dslv-1,2]==cohs) {dslv=dslv-1}	
          else {
            if (sds[dslv-1,2]==2*cohs) {sds[dslv-1,4]=dslv-1; icoh=ncoh1}
          }	
          }
        }
      }
    }
    
    mtd=max(sds[,4])
    Rest33[ib,1]=sum(sds[3:nds2-1,1])
    Rest33[ib,2]=sum(sds[3:nds2-1,2])
    Rest33[ib,3]=sum(sds[2:mtd,1])
    Rest33[ib,4]=sum(sds[2:mtd,2])
    Rest33[ib,5]=mtd-1
    Resd33[ib,,]=sds[3:nds2-1,1:2]
    
    ## mTPI
    sdsT=matrix(0,nds2,5); sdsT[nds2,3]=4				## Escalation summary by doses
    colnames(sdsT)=c("dlts","n","esc","MTD","")
    ## modified from here ------------------------------------- 
    ## dslv = 1 is a helper for start, not an actual dose
    # icoh=0; dslv=1; nchi=0;							## cohort i, dose i, cohorts at dose i
    icoh <- 0; dslv <- dslv_start + 1; nchi <- 0
    sdsT[1,1]=sdsT[1,1]+dltc[1,1]             
    sdsT[1,2]=sdsT[1,2]+cohs  
    icoh <- 1
    ## end of modification -----------------------------------
    while (icoh < ncoh1)	{
      icoh=icoh+1
      sdsT[dslv,1]=sdsT[dslv,1]+dltc[dslv,icoh]              ## #.of DLT 
      sdsT[dslv,2]=sdsT[dslv,2]+cohs                          ## #.of pt treated 
      Esci=Escf[sdsT[dslv,1]+1,sdsT[dslv,2]]                  ## get dose decision from spreadsheet
      if (Esci==2 & sdsT[dslv,2]>=ncohd) Esci=1               ## capped, make S to E; no impact as this has been handled in setup
      ttrch=ttrch+1;if (sdsT[dslv,2]>=ncohd) rchcap=rchcap+1 
      sdsT[dslv,3]=Esci
      if (sdsT[dslv,3]==1)	{
        
        # for escalation: 3 conditions for stop
        # 1. reach max dose level; 2. DU at next dose level; 3. next dose level reach cap size
        # change made ---------------------------------------
        # if (dslv==nds1|sdsT[dslv+1,3]==4)	{
        #   if (sdsT[dslv,2]>=ncohd) {icoh=ncoh1}	}
        if (dslv == nds1 & sdsT[dslv, 2] >= ncohd | # at the highest dose and the cohort size reaches cap
            dslv < nds1 & sdsT[dslv+1,3]==4 | # not the highest dose, but next level is DU
            sdsT[dslv+1,2]>=ncohd)	{ # the next cohort reaches cap size
          icoh=ncoh1
        } else if (dslv == nds1 & sdsT[dslv, 2] < ncohd){ # reach highest dose level but not cap size
          dslv <- dslv  # stay at current dose
        }
        else {dslv=dslv+1}
        ## end of change -------------------------------------
        
      }	else	{
        if (sdsT[dslv,3]>=3) {
          if (dslv >2)	{
            if (sdsT[dslv-1,2]>=ncohd) {icoh=ncoh1} else {dslv=dslv-1}	
          }	else	{
            p.v1=1-stats::pbeta(pt,apr+sdsT[dslv,1],bpr+sdsT[dslv,2]-sdsT[dslv,1])
            # update this part to account for the case where cohort 1 has 12, but p.v1 < p.ud
            # original version ----------------------------
            # if (p.v1>=p.ud) {icoh=ncoh1}	
            # updated to be ---------------------------------
            if (p.v1>=p.ud | sdsT[dslv, 2] >= ncohd) {icoh=ncoh1}	
            # end of update ---------------------------------
          }
        }
      }
    }
  #  if(max(sdsT[, 2])> ncohd){ print(ib); print(sdsT)}
    ## original  ----------------------------------------------
    # mn=sum(sdsT[,2]>0)                                   ## #.of dose tested
    # ai=sdsT[1:mn,1]; ni=sdsT[1:mn,2]; dltr=ai/ni         ## DLT_hat in each dose level
    # y.hat=numeric(mn); x=1:mn;	iso=1
    # 
    
    ## changed to  ----------------------------------------------
    
    test_level <- which(sdsT[,2]> 0)
    mn <- length(test_level)
    ai <- sdsT[test_level, 1]; ni <- sdsT[test_level, 2]; dltr <- ai/ni
    y.hat <- numeric(mn); x <- 1:mn; iso <- 1
    
    ## end of change ----------------------------------------------
    while(iso <= mn)	{
      
      x1=x[iso:mn]; w1=ni[iso:mn]; y1=dltr[iso:mn]
      cwm=cumsum(w1*y1)/cumsum(w1)                   ## cumulative dlt/cumulative n
      iso1=which.min(cwm)+iso-1
      y.hat[iso:iso1]=min(cwm); iso=iso1+1
    }	
    tdi=y.hat-pt;
    
    
    ### original version------------------------------------------------
    # #w.min=which(abs(tdi)==min(abs(tdi)));	minii=sign(tdi[w.min])
    # #if (max(minii)>=0) mtd2=w.min[which.max(minii)]
    # #if (max(minii)<0) mtd2=utils::tail(w.min,1)
    # ########################################################5555
    # w.min=which(abs(abs(tdi)-min(abs(tdi)))<=0.000001);	minii=sign(tdi[w.min])
    # if (max(minii)<0) mtd2=utils::tail(w.min,1)
    # if (min(minii)>=0) mtd2=w.min[which.min(minii)]
    # if ((max(minii)>=0) & (min(minii)<0)) mtd2=utils::tail(w.min[which(minii<0)],1)
    ##########################################################5555
    
    ### changed to ------------------------------------------
    w.min=which(abs(abs(tdi)-min(abs(tdi)))<=0.000001);	minii=sign(tdi[w.min])
    if (max(minii)<0) mtd2=test_level[utils::tail(w.min,1)]
    if (min(minii)>=0) mtd2=test_level[w.min[which.min(minii)]]
    if ((max(minii)>=0) & (min(minii)<0)) mtd2=test_level[utils::tail(w.min[which(minii<0)],1)]
    
    ### end of change ------------------------------------------
    
    RestTPI[ib,1]=sum(sdsT[3:nds2-1,1]) # total number of dlts
    RestTPI[ib,2]=sum(sdsT[3:nds2-1,2]) # total number of subjects
    RestTPI[ib,3]=sum(sdsT[2:mtd2,1])    # total number of dlts up to MTD
    RestTPI[ib,4]=sum(sdsT[2:mtd2,2])    # total number of subjects up to MTD
    RestTPI[ib,5]=mtd2 - 1                  # the dose limiting toxicity
    RestTPI[ib,6]=utils::tail(tdi,1)+pt
    ResdTPI[ib,,1:2]=sdsT[3:nds2-1,1:2]
    mtd2=mtd2-1
    ## orignial version -------------------------------
    # ResdTPI[ib,,3]=head(c(y.hat[-1],rep(0,nds)),nds)
    ## changed to -------------------------------------
    iso_vec <- rep(0, nds)
    iso_vec[test_level[-1]-1] <- y.hat[-1]
    ResdTPI[ib, , 3] <- iso_vec
    ## end of change
  }
  
  ## mTPI summary by dose
  PerDS=as.data.frame(matrix("",nds,10))
  
  PerDS[,1]=sprintf("%3.0f",1:length(tdose))					# doses
  
  #  PerDS[,2]=sprintf("%2.2f",trsk)							# tox risks
  PerDS[,2]= trsk
  mtd.dis=table(RestTPI[,5][RestTPI[,5]>0])/B
  
  ##############################################1111
  mtd.dno=as.integer(rownames(mtd.dis))	
  mtd.dit=numeric(nds); mtd.dit[mtd.dno]=mtd.dis
  # PerDS[,3]=sprintf("%3.1f",100*mtd.dit)						# MTD selection(%)  
  PerDS[,3]= 100*mtd.dit						# MTD selection(%)
  ##############################################1111
  
  test.r=colMeans(ResdTPI[,,2]>0)
  # PerDS[,4]=sprintf("%3.1f",100*test.r)						# being tested (%)
  PerDS[,4] = 100*test.r 						# being tested (%)
  iso.r=colMeans(ResdTPI[,,3])
  #  PerDS[,5]=sprintf("%2.2f",iso.r/test.r)					# mean iso estimates
  PerDS[,5]= iso.r/test.r					# mean iso estimates
  ncoh.n=colMeans(ResdTPI[,,2])
  # PerDS[,6]=sprintf("%2.2f",ncoh.n)							# mean # of subjects
  PerDS[,6]= ncoh.n							# mean # of subjects
  ntox.n=colMeans(ResdTPI[,,1])
  # PerDS[,7]=sprintf("%2.2f",ntox.n)							# mean toxicities
  # PerDS[,8]=sprintf("%2.2f",ntox.n/ncoh.n)					# mean tox rates
  # PerDS[,9]=sprintf("%2.2f",ncoh.n/test.r)					# mean # of subjects in treated dose cohorts
  # PerDS[,10]=sprintf("%2.2f",ntox.n/test.r)					# mean toxicities in tested dose cohorts
  PerDS[,7]= ntox.n							# mean toxicities
  PerDS[,8]= ntox.n/ncoh.n					# mean tox rates
  PerDS[,9]= ncoh.n/test.r					# mean # of subjects in treated dose cohorts
  PerDS[,10]= ntox.n/test.r					# mean toxicities in tested dose cohorts
  
  ## 3+3 summary by dose
  P33DS=as.data.frame(matrix("",nds,7))
  P33DS[,1]=sprintf("%3.1f",tdose)					# doses
  P33DS[,2]=sprintf("%2.2f",trsk)							# tox risks
  mtd.dis=table(Rest33[,5][Rest33[,5]>0])/B
  ##############################################2222
  mtd.dno=as.integer(rownames(mtd.dis))  	
  mtd.dit=numeric(nds); mtd.dit[mtd.dno]=mtd.dis
  P33DS[,3]=sprintf("%3.1f",100*mtd.dit)						# MTD selection(%)
  ##############################################2222
  # MTD selection(%)
  test.r=colMeans(Resd33[,,2]>0)
  P33DS[,4]=sprintf("%3.1f",100*test.r)						# being tested (%)
  ncoh.n=colMeans(Resd33[,,2])
  P33DS[,5]=sprintf("%2.2f",ncoh.n)							# mean # of subjects
  ntox.n=colMeans(Resd33[,,1])
  P33DS[,6]=sprintf("%2.2f",ntox.n)							# mean toxicities
  P33DS[,7]=sprintf("%2.2f",ntox.n/ncoh.n)					# mean tox rates
  
  ### mTPI Summary per trial
  PerTR=as.data.frame(array(0,c(4,5)))
  ncoh.s=rowSums(ResdTPI[,,2])
  PerTR[1,]=c(range(ncoh.s),stats::median(ncoh.s),mean(ncoh.s),stats::sd(ncoh.s))[c(1,3,4,2,5)]
  ntox.s=rowSums(ResdTPI[,,1])
  PerTR[2,]=c(range(ntox.s),stats::median(ntox.s),mean(ntox.s),stats::sd(ntox.s))[c(1,3,4,2,5)]
  mds=sum(trsk<=pt)+1
  ##############################################3333
  if(mds<nds)  ncoh.s=apply(ResdTPI[,mds:nds,2],1,sum) 
  if(mds==nds) ncoh.s=ResdTPI[,mds,2] 
  if(mds>nds)  ncoh.s=rep(0,B)
  PerTR[3,]=c(range(ncoh.s),stats::median(ncoh.s),mean(ncoh.s),stats::sd(ncoh.s))[c(1,3,4,2,5)]
  if(mds<nds)  ntox.s=apply(ResdTPI[,mds:nds,1],1,sum)
  if(mds==nds) ntox.s=ResdTPI[,mds,1]
  if(mds>nds)  ntox.s=rep(0,B)
  ##############################################3333
  #	ntox.s=rowSums(ResdTPI[,mds:nds,1])
  PerTR[4,]=c(range(ntox.s),stats::median(ntox.s),mean(ntox.s),stats::sd(ntox.s))[c(1,3,4,2,5)]
  PerTR[,c(3,5)]=round(PerTR[,c(3,5)],2)
  
  ### 3+3 Summary per trial
  P33TR=as.data.frame(array(0,c(4,5)))
  ncoh.s=rowSums(Resd33[,,2])
  P33TR[1,]=c(range(ncoh.s),stats::median(ncoh.s),mean(ncoh.s),stats::sd(ncoh.s))[c(1,3,4,2,5)]
  ntox.s=rowSums(Resd33[,,1])
  P33TR[2,]=c(range(ntox.s),stats::median(ntox.s),mean(ntox.s),stats::sd(ntox.s))[c(1,3,4,2,5)]
  ################################################4444
  if(mds<nds)  ncoh.s=apply(Resd33[,mds:nds,2],1,sum) 
  if(mds==nds) ncoh.s=Resd33[,mds,2] 
  if(mds>nds)  ncoh.s=rep(0,B)
  #	ncoh.s=rowSums(Resd33[,mds:nds,2])
  P33TR[3,]=c(range(ncoh.s),stats::median(ncoh.s),mean(ncoh.s),stats::sd(ncoh.s))[c(1,3,4,2,5)]
  if(mds<nds)  ntox.s=apply(Resd33[,mds:nds,1],1,sum) 
  if(mds==nds) ntox.s=Resd33[,mds,1] 
  if(mds>nds)  ntox.s=rep(0,B)
  ################################################4444
  #	ntox.s=rowSums(Resd33[,mds:nds,1])
  P33TR[4,]=c(range(ntox.s),stats::median(ntox.s),mean(ntox.s),stats::sd(ntox.s))[c(1,3,4,2,5)]
  P33TR[,c(3,5)]=round(P33TR[,c(3,5)],2)
  ### Early termination
  Eterm=c(mean(RestTPI[,5]==0),mean(Rest33[,5]==0))
  #print(ttrch);print(rchcap);print(rchcap/ttrch);
  pctcap=rchcap/ttrch
  return(list(PerDS=PerDS,P33DS=P33DS,PerTR=PerTR,P33TR=P33TR,Eterm=Eterm,pctcap=pctcap))
}


########################################################################################
## plot
########################################################################################

#' @title Bayes Decision Table for mTPI2
#' @details For each cohort size and number of DLTs observed, calculate the
#'   Bayes posterior probability, based on which a decision table is generated
#'   using mTPI2 method.
#' @references{
#'   \insertRef{guo2017bayesian}{shinyapps4clinicaltrial}
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


########################################################################################
## simulation
########################################################################################

#' Run the simulation
#'
#' @param tdose true DLT probabilities
#' @param ndose total number of doses
#' @param nmax max number of subjects 
#' @param cosize the cohort size
#' @param target target toxicity level
#' @param tolerance1 Equivalence Radius -(target tox - tolerance1 = lower acceptance value )
#' @param nsim number of simulations to run
#' @param tolerance2 Equivalence Radius +(target tox + tolerance1 = lower acceptance value )
#' @param cocap max number of subjects for each cohort
#' @param tox Unacceptable Toxicity: Prob(Overdosing)
#' @param a,b prior parameters for beta distribution 
#' @param dslv_start Starting Dose to run simulation
#'
#' @return a list 
#' @export
#'
#' @examples
#' sim_mtpi2(tdose = c(0.05,0.1,0.2,0.25,0.3,0.35), ndose = 6,nmax = 30,
#' cosize = 3,target = 0.3, tolerance1 = 0.05,nsim = 1000, tolerance2= 0.05, 
#' cocap = 12,tox = 0.95,  a = 1, b = 1, dslv_start = 3)
sim_mtpi2 = function(tdose, ndose, nmax, cosize, target, tolerance1, nsim,  
                     tolerance2, cocap, tox,  a, b, dslv_start) {
  
  nds <- length(tdose)
  nds1 <- nds+1 
  nds2 <- nds+2
  if(ndose!=length(tdose)) stop("ERROR: Number of doses do not match input.")
  if(min(tdose)<=0) stop("ERROR: Dose input must be positive numbers")
  if(max(tdose)>=1) stop("ERROR: Dose input must be less than 1")
  
  mxn <- nmax 
  cohs0 <- cosize
  mxn1 <- mxn+1						## max N
  ncoh <- mxn/cohs0 
  ncoh1 <- ncoh+1						## no. of cohorts
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
  
  #    print(c(cohs0,mxn,ncoh,pt,pt1,pt2,tdose,nds,nds1,nds2,ncoh1))
  outlist <- mTPIeval(B=nsim,scen=1,cohs=cohs0,ncohd=cocap,ncoh=ceiling(ncoh),
                      mxn=mxn,pt=pt,pt.a=pt1,pt.b=pt2,tdose=tdose,nds=nds,ncoh1=ceiling(ncoh1),
                      nds1=nds1,nds2=nds2,Escf=Escf,apr=apr,bpr=bpr,p.ud=p.ud, dslv_start = dslv_start)
  outlist
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
estimate_dlt_isoreg = function(cohort_size, n_dlt, target){
  
  
  
  Doses <- 1:length(cohort_size)
  indat <- data.frame(Doses, n = cohort_size ,DLTs = n_dlt)
  
  mn <- sum(indat$n>0)
  raw.e <- indat[1:mn,3]/indat[1:mn,2]
  Iso.e <- numeric(mn) 
  iso <- 1 
  mtd <- rep("",mn)
  while(iso <= mn)	{
    w1 <- indat[iso:mn,2] 
    y1 <- raw.e[iso:mn]
    cwm <- cumsum(w1*y1)/cumsum(w1)
    iso1 <- which.min(cwm)+iso-1
    Iso.e[iso:iso1] <- min(cwm) 
    iso <- iso1+1	
  }
  Iso.e <- Iso.e+1:mn*1e-6; 
  
  tdi <- Iso.e-target;
  w.min <- which(abs(abs(tdi)-min(abs(tdi)))<=0.000001);	
  minii <- sign(tdi[w.min])
  if (max(minii)<0) MTDds <- utils::tail(w.min,1)
  if (min(minii)>=0) MTDds <- w.min[which.min(minii)]
  if ((max(minii)>=0) & (min(minii)<0)) MTDds <- utils::tail(w.min[which(minii<0)],1)
  mtd[MTDds] <- "MTD"
  raw.est <- round(raw.e,4) 
  Iso.est <- round(Iso.e,4)
  cbind(indat,raw.est,Iso.est,mtd)
}



