
## BOIN Decision Matrix
boin_decision <- function(target_tox, cohort_cap, ...){
  
  bound <- BOIN::get.boundary(target = target_tox, ncohort = 100, 
                              cohortsize = 3)
  
  temp2 <- bound$full_boundary_tab %>% as_tibble() %>% 
    mutate(type = c("nsbj", "E", "D", "DU")) %>%
    gather(key = "nsbj", value = "DLT", -type) %>% 
    mutate(nsbj = as.numeric(str_extract(nsbj, "(\\d)+"))) %>%
    filter(type != "nsbj") %>% arrange(nsbj, DLT)
  temp2 <- temp2[complete.cases(temp2), ]
  
  nsbj0 <- min(max(temp2$nsbj), cohort_cap)
  temp3 <- purrr::map_df(1:nsbj0, .f = function(i) 
    bind_rows(tibble::tibble(nsbj = i, DLT = 0:i)))
  
  temp4 <- right_join(temp2 %>% arrange(nsbj, DLT), temp3, by = c("nsbj", "DLT"))
  temp5 <- temp2 %>% filter(nsbj <= nsbj0)%>% spread(key = type, value = DLT)
  temp6 <- left_join(temp5, temp3, by = "nsbj") %>% 
    mutate(decision = case_when(
      DLT <= E ~ "E",
      E < DLT & DLT < D ~ "S",
      DLT >= D & (DLT < DU | is.na(DU)) ~ "D", 
      DLT >= DU ~ "DU"
    ))
  
  temp6 <- temp6 %>% select(nsbj, DLT, decision) %>% 
    spread(key = nsbj, value = decision)
  
  return(temp6)
}


## 3 + 3 decision matrix
hybrid33_decision <- function(){
  
  decision <- tibble::tibble(DLT = seq(0, 6, by = 1), 
                             `3` = c("E", "S", "DU", "DU", rep(NA, 3)),
                             `6` = c("E", "E", rep("DU", 5)))
  return(decision)
}

