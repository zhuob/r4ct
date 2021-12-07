#' The risk table from that survival curve
#'
#' A dataset containing time, and number of subjects at risk
#'
#' @format A data frame with 10 rows and 4 variables:
#'  - *time*: time along the axis of the survival curve (starting from 0)
#'  - *Ipilimumab*: Number of subjects at risk from Ipilimumab arm
#'  - *`Pembrolizumab, Q2W`*: Number of subejcts at risk from `Pembrolizumab, Q2W` arm
#'  - *`Pembrolizumab, Q3W`*: number of subjects at risk from `Pembrolizumab, Q3W` arm 
"risk_table"