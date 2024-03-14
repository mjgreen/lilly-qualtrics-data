suppressPackageStartupMessages(library(tidyverse))
dd <- 
  read_csv(
    "JASP data Attractiveness and gender influence on morality.csv",
    show_col_types = F) %>% 
  select_if(function(x){!all(is.na(x))}) %>% 
  mutate(pnum=seq_along(`participant gender`)) %>% 
  dplyr::rename(
    page = `participant age`, 
    psex=`participant gender`, 
    mval=`morality judgment_SC0`
    ) %>% 
  relocate(pnum, page, psex, mval) %>% 
  mutate(psex=str_to_lower(psex)) %>% 
  mutate(psex=as_factor(psex)) 


dd <- dd %>% 
  rename(
    scenario_1 = `scenario 1 _1`, 
    # notice the special character, maybe a non-breaking space
    scenario_2 = "Scenario 2_1",
    scenario_3 = "scenario 3_1",
    scenario_4 = "Scenario 4_1",
    scenario_5 = "Scenario 5_1",
    scenario_6 = "scenario 6_1"
    ) 

dd <- dd %>% 
  rename(
    "unattractive F10+M_1" = "unattractive F+M10_1" ,
    "unattractive F10+M_2" = "unattractive F+M10_2" ,
    "unattractive F10+M_3" = "unattractive F+M10_3" ,
    "unattractive F10+M_4" = "unattractive F+M10_4" ,
    "unattractive F10+M_5" = "unattractive F+M10_5" ,
    "unattractive F10+M_6" = "unattractive F+M10_6" 
  )

dd <- dd %>% 
  rename(
    "unattractive male 2_1" = "unattractive male _1" 
    # notice the special character, maybe a non-breaking space
  )

## ATTRACTIVENESS / MORALITY / SCENARIO

