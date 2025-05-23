# method for generating correlated ORR and OS

#' Correlated OS and ORR derivation
#' @description Derive parameters for correlated ORR and OS based on known
#' quantities.
#'
#' @param p0 response rate for control
#' @param p1 response rate for treatment
#' @param m0 median survival for control
#' @param m1 median survival time for treatment
#' @param rho1 hazard ratio for the Responder vs Non-Responder
#' @param rho2 additional hazard ratio for Treatment vs Control
#' @details  Based on difference in hazard function between responder vs
#'   Non-Responder, and Treatment Vs Control, derive the corresponding
#'   parameters. Let \eqn{X} represent treatment, with \eqn{X = 1} being
#'   \code{Treatment} and \eqn{X = 0} being \code{Control}, \eqn{Y = 1}
#'   represents a response and \mjseqn{Y = 0} a non-response, and \eqn{T} the
#'   survival time. Suppose the following table summarizes the hazard rate for
#'   each group 
#' \loadmathjax
#'
#'   |           | Non-Responder (Y=0)       |  Responder(Y=1)              |
#'   |---------- | --------------       | --------------------------| 
#'   |Control (X=0)   | \mjseqn{\lambda_0}      | \mjseqn{\rho_1\lambda_0}     | 
#'   | Treatment(X=1)    |  \mjseqn{\rho_2\lambda_0}|\mjseqn{\rho_1\rho_2\lambda_0}|
#'
#'   where \mjseqn{\lambda_0} is the harzard rate for non-responders in the
#'   control group, and the survival time follows exponential distribution. Then
#'   \mjseqn{T |X = 0, Y = 0 \sim \exp(\lambda_0)} and \mjseqn{T|X = 1, Y =
#'   0\sim \exp(\rho_2\lambda_0)}, and so on. Following \mjsdeqn{P(T\leq t| X=1)
#'   = P(T\leq t|Y=0,X=1)\cdot P(Y=0|X=1) + P(T\leq t|Y=1,X=1)\cdot P(Y=1|X=1),} 
#'   it can be shown that \mjsdeqn{P(T\leq t|X=1) =
#'   (1-\exp(-\rho_2\lambda_0t))(1-p_1) + (1-\exp(-\rho_1\rho_2\lambda_0t))p_1}
#'   Suppose \mjseqn{p_0,p_1} are the response rate, \mjseqn{m_0, m_1} are the
#'   median survival time for control and Treatment arm, then the following
#'   equations hold \mjdeqn{\texttt{For
#'   Treatment:~~}\[1-\exp(-\lambda_0\rho_1\rho_2m_1)\]p_1 +
#'   \[1-\exp(-\lambda_0\rho_2m_1)\](1-p_1) = \frac{1}{2}}{ASCII representation}
#'   \mjdeqn{\texttt{For Control:~~}\[1-\exp(-\lambda_0\rho_1m_0)\]p_0 +
#'   \[1-\exp(-\lambda_0m_0)\](1-p_0) = \frac{1}{2}}{ASCII representation} Given
#'   \mjseqn{p_0, p_1, m_0, m_1} and \mjseqn{\rho_1}, the quantities
#'   \mjseqn{\rho_2} and \mjseqn{\lambda_2} can be solved.
#'
#'   This model leads to evaluation of survival time by treatment assignment and
#'   response status. Suppose that the randomization ratio is \mjseqn{1:r_0}
#'   for \code{Control:Treatment} arm respectively, then \mjseqn{P(T> t|Y = 1) =
#'   P(T> t|X=0, Y=1)\cdot P(X=0|Y=1) + P(T>t|X=1,Y=1)\cdot P(X=1|Y=1)}. Note
#'   \mjseqn{P(X=0|Y=1) = P(X=0)} because treatment assignment is independent of
#'   response status. Then it follows that for survival function
#'   \mjsdeqn{\texttt{For Responder:~~ }P(T> t|Y = 1) = \frac{1}{1 +
#'   r_0}\cdot\exp(-\lambda_0\rho_1t) + \frac{r_0}{1 +
#'   r_0}\cdot\exp(-\lambda_0\rho_1\rho_2t)} \mjsdeqn{\texttt{For
#'   Non-responder:~~}P(T> t|Y=0) = \frac{1}{1+r_0}\cdot\exp(-\lambda_0t) +
#'   \frac{r_0}{1+r_0}\cdot\exp(-\lambda_0\rho_2t)} Applying similar logic we
#'   can obtain \mjsdeqn{\texttt{Treatment:~~}P(T>t|X=1) =
#'   p_1\cdot\exp(-\lambda_0\rho_1\rho_2t) +
#'   (1-p_1)\cdot\exp(-\lambda_0\rho_2t)}
#'   \mjsdeqn{\texttt{Control:~~}P(T>t|X=0)=p_0\cdot\exp(-\lambda_0\rho_1t) +
#'   (1-p_0)\cdot\exp(-\lambda_0t)}
#' @return A tibble containing the values of \code{rho1, rho2, lambda0} and the
#'   median survival time by group.
#' @export
#'
#' @examples
#' # Calculate the parameters
#'  x1 <- find_hazard(p0 = 0.25, p1 = 0.45, m0 = 8.5, m1 = 17, rho1 = 0.1)
#'
#' # --------------------------------------------------------------------------
#' # Calculate median survival time for treatment/control and responder/non-responder
#' # --------------------------------------------------------------------------
#' # Responder survival time
#' f1 <- function(t){
#'   1/(1+r0)*exp(-lambda0*rho1*t) + (r0/(1+r0))*exp(-lambda0*rho1*rho2*t) - 0.5
#' }
#' # non-Responder
#' f2 <- function(t){
#'   1/(1+r0)*exp(-lambda0*t) + (r0/(1+r0))*exp(-lambda0*rho2*t) - 0.5
#' }
#' 
#' # Treatment 
#' f3 <- function(t){
#'   p1*exp(-lambda0*rho1*rho2*t) + (1-p1)*exp(-lambda0*rho2*t) - 0.5
#' }
#' 
#' # Control
#' f4 <- function(t){
#'   p0*exp(-lambda0*rho1*t) + (1-p0)*exp(-lambda0*t) - 0.5
#' }
#' 
#' p0 <- 0.25; p1 <- 0.45;  r0 <- 1;
#' x1 <- find_hazard(p0, p1, m0 = 8.5, m1 = 17, rho1 = 0.5, rho2 = NA)
#' lambda0 <- x1$lambda0; rho1 <- x1$rho1; rho2 <- x1$rho2; 
#' print.data.frame(x1)
#' # solve for median survival time
#' uniroot(f1, interval = c(0, 1e4), tol = 1e-2)$root # Responder
#' uniroot(f2, interval = c(0, 1e4), tol = 1e-2)$root # Non-Responder
#' uniroot(f3, interval = c(0, 1e4), tol = 1e-2)$root # Treatment
#' uniroot(f4, interval = c(0, 1e4), tol = 1e-2)$root # Control
#'
#'
#' # -------------------------------------------------------------------------
#' # next part is a numerical and visual presentation of relationship between
#' # orr/treatment/response 
#' # -------------------------------------------------------------------------
#' # hazard function for the control arm 
#' hz_fun <- function(lambda0, rho1, time, p0, p1 = NA, rho2 = NA){
#'   
#'   if(is.na(rho2) & is.na(p1)){ # calculate hazard function for control arm
#'     # density function 
#'     ft <- (1-p0)*lambda0*exp(-lambda0*time) + 
#'       p0*rho1*lambda0*exp(-rho1*lambda0*time)
#'     # survival function
#'     st <- (1-p0)*exp(-lambda0*time) + p0*exp(-rho1*lambda0*time)
#'   } else{
#'     ft <- (1-p1)*rho2*lambda0*exp(-rho2*lambda0*time) + 
#'       p1*rho1*rho2*lambda0*exp(-lambda0*rho1*rho2*time)
#'     st <- (1-p1)*exp(-rho2*lambda0*time) + p1*exp(-rho1*rho2*lambda0*time)
#'   }
#'   
#'   return(ft/st)
#' }
#' 
#' # Calculate survival probabilites at each given time and scenario
#' surv_fun <- function(time, r0, rho1, rho2, lambda0, p0, p1){
#'   
#'   # for response status 
#'   # for responder
#'   st_resp1 <- 1/(r0+1)*exp(-lambda0*rho1*time) + 
#'     r0/(1+r0)*exp(-lambda0*rho1*rho2*time) 
#'   # for non-responder
#'   st_resp0 <- 1/(r0+1)*exp(-lambda0*time) + r0/(1+r0)*exp(-lambda0*rho2*time)
#'   
#'   # for treatment 
#'   st_trt <- p1*exp(-lambda0*rho1*rho2*time) + (1-p1)*exp(-lambda0*rho1*time)
#'   # for control
#'   st_ctrl <- p0*exp(-lambda0*rho1*time) + (1-p0)*exp(-lambda0*time)
#'   
#'   result0 <- tibble::tibble(st_resp0, st_resp1, st_trt, st_ctrl)
#'   
#'   return(result0)
#' }
#' 
#' 
#' # plot orr and survival 
#' 
#' plot_orr_surv <- function(rho1, rho2, lambda0, p0, p1, r0, 
#'                           time = seq(1, 60, by = 0.1)){
#'   
#'   hz0 <- hz_fun(lambda0 = lambda0, rho1 = rho1, time = time, 
#'                 p0 = p0, p1 = NA, rho2 = NA)
#'   hz1 <- hz_fun(lambda0 = lambda0, rho1 = rho1, time = time, 
#'                 p0 = p0, p1 = p1, rho2 = rho2)
#'   
#'   surv_prob <- surv_fun(time = time, r0 = r0, rho1 = rho1, rho2 = rho2, 
#'                         lambda0 = lambda0, p0 = p0, p1 = p1)
#'   
#'   result <- dplyr::bind_cols(
#'     tibble::tibble(time = time, hz0 = hz0, hz1 = hz1),surv_prob)
#'   
#'   result <- dplyr::mutate(result, hr = hz1/hz0)
#'   
#'   res_long <- result %>% tidyr::pivot_longer(cols = 2:8, names_to = "type", 
#'                                              values_to = "value")  %>%
#'     dplyr::mutate(category = dplyr::case_when(
#'       stringr::str_detect(type, "hz") ~ "Hazard rate", 
#'       stringr::str_detect(type, "resp") ~ "Response status",
#'       stringr::str_detect(type, "trt|ctrl") ~ "Arm", 
#'       stringr::str_detect(type, "hr")  ~ "Hazard ratio"
#'     ))
#'   
#'   res_long2 <- res_long %>% split(f = res_long$category)
#'   
#'   library(ggplot2)
#'   p1 <- ggplot(data = res_long2$Arm, aes(x = time, y = value)) + 
#'     geom_line(aes(color = type)) + 
#'     facet_wrap(facets = "category", scale = "free")
#'   
#'   p2 <- p1 %+% res_long2$`Hazard rate`
#'   p3 <- p1 %+% res_long2$`Hazard ratio`
#'   p4 <- p1 %+% res_long2$`Response status`
#'   
#'   obj <- list(p1, p2, p3, p4)
#'   return(obj)
#'   
#' }
#'
#' # Plot that shows survival curve by treatment/response status, and hazard.
#'  x1 <- find_hazard(p0 = 0.25, p1 = 0.45, m0 = 8.5, m1 = 17, rho1 = 0.1)
#' plot_orr_surv(rho1 = x1$rho1, rho2 = x1$rho2, lambda0 = x1$lambda0, p0 =
#'  0.25, p1 = 0.45, r0 = 1)
#'
#' 
find_hazard <- function(p0, p1, m0, m1, rho1 = NA, rho2 = NA){
  
  obj_fun3 <- function(lambda0, rho1, rho2){
    
    f1 <- (1-exp(-lambda0*rho1*m0))*p0 + (1-exp(-lambda0*m0))*(1-p0)
    f2 <- (1-exp(-lambda0*rho1*rho2*m1))*p1 + (1-exp(-lambda0*rho2*m1))*(1-p1)
    
    return(c(f1, f2))
  }
  
  if(is.na(rho1)){
    if(is.na(rho2)){ stop("rho2 needs to be specified if rho1 is 'NA'.")}
    obj_fun4 <- function(x) crossprod(obj_fun3(x[1], x[2], rho2 = rho2) - c(0.5, 0.5))
  } else if (is.na(rho2)){
    if(is.na(rho1)){ stop("rho1 needs to be specified if rho2 is 'NA'.")}
    obj_fun4 <- function(x) crossprod(obj_fun3(x[1], x[2], rho1 = rho1) - c(0.5, 0.5))
  }
  
  res <- stats::optim(c(0.2, 0.5), obj_fun4)
  lambda0_est <- res$par[1];
  rho1_est <- ifelse(is.na(rho1), res$par[2], rho1)
  rho2_est <- ifelse(is.na(rho2), res$par[2], rho2)
  if(rho1_est > 1 ){
    warning("Calculated rho1 > 1, meaning responder group has shorter OS. Please check if this is appropriate!")
  } 
  if(rho2_est > 1){
    warning("Calculated rho2 > 1, meaning Treatment arm has shorter OS, please check if this is appropriate!!")
  }
  
  mos <- log(2)/c(lambda0_est*c(1, rho1_est, rho2_est, rho1_est*rho2_est))
  
  outcome_profile <- tibble::tibble(
    rho1 = rho1_est, rho2 = rho2_est, lambda0 = lambda0_est, 
    `Control/Non-Responder`    = mos[1],
    `Control/Responder`        = mos[2],
    `Treatment/Non-Responder`  = mos[3],
    `Treatment/Responder`      = mos[4]
  )
  
  return(outcome_profile) 
}

# # hazard function for the control arm 
# hz_fun <- function(lambda0, rho1, time, p0, p1 = NA, rho2 = NA){
#   
#   if(is.na(rho2) & is.na(p1)){ # calculate hazard function for control arm
#     # density function 
#     ft <- (1-p0)*lambda0*exp(-lambda0*time) + 
#       p0*rho1*lambda0*exp(-rho1*lambda0*time)
#     # survival function
#     st <- (1-p0)*exp(-lambda0*time) + p0*exp(-rho1*lambda0*time)
#   } else{
#     ft <- (1-p1)*rho2*lambda0*exp(-rho2*lambda0*time) + 
#       p1*rho1*rho2*lambda0*exp(-lambda0*rho1*rho2*time)
#     st <- (1-p1)*exp(-rho2*lambda0*time) + p1*exp(-rho1*rho2*lambda0*time)
#   }
#   
#   return(ft/st)
# }
# 
# surv_fun <- function(time, r0, rho1, rho2, lambda0, p0, p1){
#   
#   # for response status 
#   # for responder
#   st_resp1 <- 1/(r0+1)*exp(-lambda0*rho1*time) + 
#     r0/(1+r0)*exp(-lambda0*rho1*rho2*time) 
#   # for non-responder
#   st_resp0 <- 1/(r0+1)*exp(-lambda0*time) + r0/(1+r0)*exp(-lambda0*rho2*time)
#   
#   # for treatment 
#   st_trt <- p1*exp(-lambda0*rho1*rho2*time) + (1-p1)*exp(-lambda0*rho1*time)
#   # for control
#   st_ctrl <- p0*exp(-lambda0*rho1*time) + (1-p0)*exp(-lambda0*time)
#   
#   result0 <- tibble::tibble(st_resp0, st_resp1, st_trt, st_ctrl)
# 
#   return(result0)
# }
# 
# 
# # plot orr and survival 
# 
# plot_orr_surv <- function(rho1, rho2, lambda0, p0, p1, r0, 
#                           time = seq(1, 60, by = 0.1)){
#   
#   hz0 <- hz_fun(lambda0 = lambda0, rho1 = rho1, time = time, 
#                 p0 = p0, p1 = NA, rho2 = NA)
#   hz1 <- hz_fun(lambda0 = lambda0, rho1 = rho1, time = time, 
#                 p0 = p0, p1 = p1, rho2 = rho2)
#   
#   surv_prob <- surv_fun(time = time, r0 = r0, rho1 = rho1, rho2 = rho2, 
#                         lambda0 = lambda0, p0 = p0, p1 = p1)
#   
#   result <- dplyr::bind_cols(
#     tibble::tibble(time = time, hz0 = hz0, hz1 = hz1),surv_prob)
#   
#   result <- dplyr::mutate(result, hr = hz1/hz0)
#   
#   res_long <- result %>% tidyr::pivot_longer(cols = 2:8, names_to = "type", 
#                                              values_to = "value")  %>%
#     dplyr::mutate(category = dplyr::case_when(
#       stringr::str_detect(type, "hz") ~ "Hazard rate", 
#       stringr::str_detect(type, "resp") ~ "Response status",
#       stringr::str_detect(type, "trt|ctrl") ~ "Arm", 
#       stringr::str_detect(type, "hr")  ~ "Hazard ratio"
#     ))
# 
#   res_long2 <- res_long %>% split(f = res_long$category)
#   
#   library(ggplot2)
#   p1 <- ggplot(data = res_long2$Arm, aes(x = time, y = value)) + 
#     geom_line(aes(color = type)) + 
#     facet_wrap(facets = "category", scale = "free")
#   
#   p2 <- p1 %+% res_long2$`Hazard rate`
#   p3 <- p1 %+% res_long2$`Hazard ratio`
#   p4 <- p1 %+% res_long2$`Response status`
#   
#   obj <- gridExtra::grid.arrange(p1, p2, p3, p4)
#   return(obj)
#   
# }




