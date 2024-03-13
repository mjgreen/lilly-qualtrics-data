library(tidyverse)
dd <- 
  read_csv("Clean_Names_JASP_data_Attractiveness_and_gender_influence_on_morality.csv", show_col_types = F) %>% 
  #select(-c(290,291,292,293,294,295,296,297,298)) %>% 
  mutate(participant=seq_along(`participant gender`)) %>% 
  relocate() %>% 
  relocate(participant, `participant age`, `participant gender`, `morality judgment_SC0`)

dd <- dd %>% pivot_longer(cols=5:290, names_to="identifier")

dd$attractiveness=NA
dd <- dd %>% mutate(attractiveness   = ifelse(substr(identifier, 1, 1) =="u", "unattractive", attractiveness))
dd <- dd %>% mutate(attractiveness   = ifelse(substr(identifier, 1, 1) =="a", "attractive", attractiveness))

dd$stimulus_gender = NA
dd <- dd %>% mutate(stimulus_gender  = ifelse(str_detect(identifier, "F"), "female", stimulus_gender))
dd <- dd %>% mutate(stimulus_gender  = ifelse(str_detect(identifier, " male"), "male", stimulus_gender))
dd <- dd %>% mutate(stimulus_gender  = ifelse(str_detect(identifier, " female"), "female", stimulus_gender))

# normalise gender entries
dd <- dd %>% mutate(`participant gender`= str_to_lower(`participant gender`) %>% as_factor())
dd <- dd %>% mutate(identifier = ifelse(str_detect(identifier, "enari"), str_to_lower(identifier), identifier))
# factorise most strings
dd <- dd %>% mutate(attractiveness = as_factor(attractiveness), stimulus_gender=as_factor(stimulus_gender))



#dd <- dd %>% mutate(scenario_present = ifelse(str_detect(identifier, "M"), "scenario_present", "rating"))

dd <- dd %>% mutate(which_morality=NA)
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_1"), "Morality 1", which_morality))
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_2"), "Morality 2", which_morality))
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_3"), "Morality 3", which_morality))
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_4"), "Morality 4", which_morality))
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_5"), "Morality 5", which_morality))
dd <- dd %>% mutate(which_morality = ifelse(str_detect(identifier, "M_6"), "Morality 6", which_morality))
dd <- dd %>% mutate(which_morality=as_factor(which_morality))

#qq=tibble(identifier=names(dd),logic=names(dd) %>% str_detect("unattractive male")) %>% filter(logic==TRUE)


# stimulus number
dd$stim_num=NA
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female1"), 1, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female2"), 2, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female3"), 3, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female4"), 4, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female5"), 5, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female6"), 6, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female7"), 7, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female8"), 8, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female9"), 9, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female10"), 10, stim_num))

dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F1"), 1, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F2"), 2, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F3"), 3, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F4"), 4, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F5"), 5, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F6"), 6, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F7"), 7, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F8"), 8, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F9"), 9, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "F10"), 10, stim_num))

dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 1"), 1, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 2"), 2, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 3"), 3, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 4"), 4, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 5"), 5, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 6"), 6, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 7"), 7, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 8"), 8, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 9"), 9, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "female 10"), 10, stim_num))

dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 1"), 1, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 2"), 2, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 3"), 3, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 4"), 4, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 5"), 5, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 6"), 6, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 7"), 7, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 8"), 8, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 9"), 9, stim_num))
dd <- dd %>% mutate(stim_num = ifelse(str_detect(identifier, "male 10"), 10, stim_num))