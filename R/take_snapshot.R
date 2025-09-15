#' Take a snapshot of simulated survival trial data
#'
#' Generic function to derive a "snapshot" of patient-level data in a 
#' simulated time-to-event trial, either by a fixed number of events 
#' or by a fixed calendar time.  
#'
#' @param x Object to dispatch on (not directly used).
#' @param by Character string specifying the snapshot method. 
#'   Options are `"time"` or `"event"`.
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A tibble containing subject-level information with derived flags 
#'   and observation times at the chosen snapshot.
#' @export
#'
#' @examples
take_snapshot <- function(by = c("time", "event"), ...){
  
  by <- match.arg(by)
  dummy <- structure(list(), class = by)
  
  UseMethod("take_snapshot", dummy)
  
}

#' Snapshot by event count
#'
#' Takes a snapshot at the calendar time corresponding to the 
#' \code{n_event}-th observed event. Events are defined as subjects 
#' whose survival time is less than or equal to dropout time.  
#'
#' @param n_event Integer. The number of events at which to take the snapshot.
#' @param accrual_time Numeric vector. Subject accrual times.
#' @param survival_time Numeric vector. Subject survival times.
#' @param dropout_time Numeric vector. Subject dropout times.
#'
#' @return A tibble with subject-level accrual, survival, dropout, 
#'   event/dropout flags, and observed time up to the snapshot.
take_snapshot.event <- function(dummy, n_event, accrual_time, survival_time, dropout_time){
  
  # form the input as a tibble
  df <- tibble::tibble(
    accrual_time = accrual_time, 
    survival_time = survival_time,
    dropout_time = dropout_time,
    time_on_calendar = accrual_time + survival_time
    )
  
  # derive the event time 
  snapshot_time <- df %>% 
    dplyr::filter(survival_time <= dropout_time) %>% 
    dplyr::arrange(time_on_calendar) %>% 
    dplyr::slice(n_event) %>% 
    dplyr::pull(time_on_calendar)
  
    snapshot <- take_snapshot.time(
      dummy,
      snapshot_time = snapshot_time,
      accrual_time = accrual_time, 
      survival_time = survival_time,
      dropout_time = dropout_time )  
  
  return(snapshot)
  
}

#' Snapshot by calendar time
#'
#' Takes a snapshot at a fixed calendar time, recording event/dropout status 
#' and time under observation for each subject at that time.  
#'
#' @param snapshot_time Numeric scalar. The calendar time at which to 
#'   take the snapshot.
#' @param accrual_time Numeric vector. Subject accrual times.
#' @param survival_time Numeric vector. Subject survival times.
#' @param dropout_time Numeric vector. Subject dropout times.
#'
#' @return A tibble with subject-level accrual, survival, dropout, 
#'   event/dropout flags, and observed time up to the snapshot.
take_snapshot.time <- function(snapshot_time, accrual_time, survival_time, dropout_time){
  

  time_on_calendar <- accrual_time + survival_time
  event_flag <- survival_time <= dropout_time  & time_on_calendar <= snapshot_time
  # for a dropout to be observed, the subject must be event free at the time
  # of dropout and the dropout time is before snapshot time
  dropout_flag <- event_flag == FALSE & survival_time >= dropout_time & 
                          dropout_time + accrual_time <= snapshot_time
  time_under_observation <- dplyr::case_when(
    event_flag == TRUE~ survival_time, 
    dropout_flag == TRUE ~ dropout_time, 
    TRUE ~ snapshot_time - accrual_time
  )
  
  snapshot <- tibble::tibble(snapshot_time, event_flag, dropout_flag, time_under_observation)
  
  return(snapshot)
  
}

library(dplyr)
library(rpact)

design0 <- rpact::getDesignGroupSequential(
  sided = 1, 
  alpha = 0.02, 
  informationRates = c(0.65, 1), 
  typeOfDesign = "asUser", 
  userAlphaSpending = c(0, 0.02)
) 
accruals <- rpact::getAccrualTime(
  maxNumberOfSubjects = 800, 
  accrualTime = c(0, 3, 6), 
  accrualIntensity = c(5, 10, 20)
)



dat <- rpact::getSimulationSurvival(
  seed = 123,
  design = design0,
  directionUpper = FALSE, 
  median2 = 17.2,
  hazardRatio = 0.79, 
  dropoutRate1 = 0.001,
  dropoutRate2 = 0.001, 
  dropoutTime = 1, 
  accrualTime = accruals,
  plannedEvents = c(390, 600),
  maxNumberOfIterations = 10,
  maxNumberOfRawDatasetsPerStage = 10
)

df1 <- rpact::getRawData(dat) %>% dplyr::filter(iterationNumber == 1)
df2 <- rpact::getData(dat) %>% dplyr::filter(iterationNumber == 1)


df3 <- take_snapshot.event(by = "event", n_event = 368, accrual_time = df1$accrualTime, survival_time = df1$survivalTime, dropout_time = df1$dropoutTime)
r4ct::run_survival(time = df3$time_under_observation, censor = df3$event_flag, arm = df1$treatmentGroup, control = 2, conf_level = 0.95)
df3 <- take_snapshot.event(566, accrual_time = df1$accrualTime, survival_time = df1$survivalTime, dropout_time = df1$dropoutTime)
r4ct::run_survival(time = df3$time_under_observation, censor = df3$event_flag, arm = df1$treatmentGroup, control = 2, conf_level = 0.95)

df4 <- take_snapshot(
  by = "event",
  n_event = 368, 
  accrual_time = df1$accrualTime, 
  survival_time = df1$survivalTime, 
  dropout_time = df1$dropoutTime
  )

