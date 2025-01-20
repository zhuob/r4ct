# r4ct 0.2.14
  * Restructure the package such that only essential functions are included 
  (removing simulation-related functions and shiny apps)
  * add function `get_tte_boundary` for converting treatment effects of survival
  endpoint in different scales 
  * add function `test_truncated_hochberg` to perform truncated Hochberg testing


# r4ct 0.2.11
  * add function `find_hazard` for derivation of parameters for correlated ORR
  and OS
  * add implementation of BOIN method for dose-escalation 
 
# r4ct 0.2.10
  * add a function `run_survival` to run survival analysis 
  
# r4ct 0.2.9
  * expand `estimate_dlt_isoreg` so that it can exclude dose levels with DU from
  estimating MTD. One `du` argument added
  
# r4ct 0.2.8
  * move function elements of `mTPI2` from the packages to the app folder

# r4ct 0.2.7
  * update function `estimate_dlt_isoreg` with results verified by `stats::isoreg`
  * expand the function `estimate_dlt_isoreg` so that it allows a cohort of size 0
  
# r4ct 0.2.6
  * fix a bug in the Farrington-Manning test
  * Add test case 
  
# r4ct 0.2.5

* change the name to `r4ct`
* add functions for Farrington-Manning test method for non-inferiority test in
the context of comparing two proportions

# shinyapps4clinicaltrial 0.2.4

* Added a `NEWS.md` file to track changes to the package.
