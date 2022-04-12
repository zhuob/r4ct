

show_reconstruct_km <- function(recon_data, data_nar){
  
  recon_km_fit <- survival::survfit(survival::Surv(time, censor) ~ arm, data = recon_data)
  
  break_time <- data_nar %>% pull(time) %>% diff() %>% unique()
  if(length(break_time) != 1){
    warning("break time in data_nar not even, please check")
  }
  
  n_arms <- recon_data %>% pull(arm) %>% unique()
  n_color <- 1:length(n_arms)
  
  recon_km <- list()
  recon_km[[1]] <- survminer::ggsurvplot(recon_km_fit,
                                         data = recon_data,
                                         risk.table = TRUE,
                                         palette = n_color,
                                         legend = c(0.86,0.9),
                                         legend.title = '',
                                         legend.labs = n_arms,
                                         title ='Reconstructed KM curve',
                                         ylab ='Survival Probability (%)',
                                         xlab ='Time (unit)',
                                         tables.y.text = TRUE,
                                         tables.y.text.col = FALSE,
                                         risk.table.title = 'Number at Risk',
                                         break.time.by = break_time,
                                         censor = TRUE,
                                         font.x = 10,
                                         font.y = 10,
                                         font.tickslab = 8,
                                         font.legend = 10,
                                         font.subtitle = 10,
                                         font.caption = 10,
                                         risk.table.fontsize = 5,
                                         tables.theme = survminer::theme_survminer(
                                           font.main = 10,
                                           font.y = 10,
                                           font.x = 10,
                                           font.tickslab = 8))
  
  res <- survminer::arrange_ggsurvplots(recon_km, print = FALSE, ncol = 1, nrow = 1, 
                                        risk.table.height = 0.25, height = 20, width = 15)
  
  return(res)
}





run_survival <- function(time, censor, arm, control=NA, alpha = 0.025){
  
  if(!is.na(control)){new_arm <- ifelse(arm == control, 0, 1)}
  
  data <- tibble::tibble(time = time, censor = censor, arm = new_arm)
  
  # log-rank test p-value
  surv <- survival::Surv(time,censor)~arm
  lr <- survival::survdiff(surv,data=data)  # run log-rank test
  lr_pvalue <- pchisq(lr$chisq, length(lr$n)-1, lower.tail = FALSE) / 2 # obtain 1-sided p-value
  
  # cox HR with 95% confidence interval
  cox <- summary(survival::coxph(surv,data=data))
  hr <- cox$coefficients[,"exp(coef)"]
  hr_lower95 <- cox$conf.int[,"lower .95"]
  hr_upper95 <- cox$conf.int[,"upper .95"]
  hr_ConfInt <- c(hr_lower95, hr_upper95)
  
  # adjust 1-sided p-value for control better than treatment case
  if (hr > 1){lr_pvalue <- 1 - lr_pvalue}
  
  # trial decision
  if (lr_pvalue < alpha & hr < 1) {decision <- 'success'} else{
    decision <- 'failure'
  }
  
  # median K-M survival
  km <- survival::survfit(surv, type="kaplan-meier", conf.type="log", data=data)
  median_arms <- as.vector(quantile(km, 0.5)$quantile)
  
  median_diff <- median_arms[2] - median_arms[1]
  
  # subjects number
  n <- sum(km$n)
  
  # events number
  events <- sum(data$censor)
  
  # median follow-up time
  data$lfu_censor <- 1 - data$censor
  surv_rev <- survival::Surv(time,lfu_censor)~arm
  follow_up <- survival::survfit(surv_rev, type="kaplan-meier", conf.type="log", data=data)
  median_fu_arms <- as.vector(quantile(follow_up, 0.5)$quantile)
  median_fu_diff <- median_fu_arms[2] - median_fu_arms[1]
  
  # mean follow-up time
  temp <- survival:::survmean(follow_up, rmean=999)
  mean_fu_arms<- temp$matrix[, "*rmean"]
  
  
  # output
  return(tibble::tibble(N=n, 
                        `N ctrl.` = sum(arm == control), 
                        `N trt.` = sum(arm != control),
                        `P Value`=round(lr_pvalue, 4), 
                        `Event` = events,
                        HR = round(hr, 3),  
                        `95% Lower`= round(hr_lower95, 3), 
                        `95% Upper`= round(hr_upper95, 3),
                        `Med. Surv. Ctrl.`= round(median_arms[1], 2), 
                        `Med. Surv. Trt.`=round(median_arms[2], 2),
                        `Med. Surv. Diff.` = round(median_diff, 2)
  )
  )
  
}






# ## get existing data from the S3 bucket
# #
#
# get_s3_data <- function(bucket = "NA",
#                         prefix = "path/to/folder"){
#
#     current_data <- aws.s3::get_bucket(bucket = bucket, prefix = prefix,
#                                        check_region = FALSE, verbose = TRUE) %>%
#         tibble::as_tibble() %>%
#         dplyr::mutate(data_avail = stringr::str_replace_all(Key, "/", " ")
#                       %>% stringr::word(3)) %>%
#         dplyr::filter(data_avail != "") %>%
#         arrange(Key)
#
# }
#

# ### add a function to update database information
#
# add_info_to_s3_bucket <- function(var_name, var_value, ...){
#
#     current_data <- get_s3_data(...)
#
#     d1 <- current_data %>% filter(stringr::str_detect(Key, "info-"))
#     d2 <- current_data %>% filter(!stringr::str_detect(Key, "info-"))
#
#     if(nrow(d1) != nrow(d2)){
#         stop("S3 bucket does not have paired table for info and data")
#     }
#     if(nrow(d1) != length(var_value)){
#         stop("new variable should have enough value for existing data sets")
#     }
#     for(i in 1:nrow(d1)){
#
#         info_file <- d1$Key[i]
#         data_file <- d2$Key[i]
#
#         ## check if the names of info and data match
#         d1_aval <- d1$data_avail[i]; d2_aval <- d2$data_avail[i]
#         if(!stringr::str_detect(d1_aval, d2_aval)){
#             stop(paste0("data ", d2_aval,  " and info file ",
#                         d1_aval, " do not match"))
#         }
#         ####### read the old data
#         dft1 <- aws.s3::s3read_using(FUN = readr::read_csv,
#                                     object = info_file,
#                                     bucket = "abc")
#
#         dft_data <- aws.s3::s3read_using(FUN = readr::read_csv,
#                                      object = data_file,
#                                      bucket = "abc")
#
#         ## change data information
#         dft1 <- dft1 %>% dplyr::mutate(new_var = var_value[i]) %>%
#         select(new_var, everything())
#         names(dft)[1] <- var_name
#
#         new_name <- paste(dft1[, 1:7], sep = "-" , collapse = "-") %>%
#             stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
#             stringr::str_replace_all(pattern = " ", replacement = "_")
#
#         new_data <- paste0(new_name, '.csv')
#         new_info <- paste0("info-", new_data)
#
#         # write new data
#         aws.s3::s3write_using(dft1, FUN = readr::write_csv, object = new_info,
#                               bucket = "abc/path/to/folder")
#
#         aws.s3::s3write_using(dft_data, FUN = readr::write_csv, object = new_data,
#                               bucket = "abc/path/to/folder")
#
#
#     }
#
# }



