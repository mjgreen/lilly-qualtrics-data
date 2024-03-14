library(tidyverse)
dd <- 
  #read_csv("Clean_Names_JASP_data_Attractiveness_and_gender_influence_on_morality.csv", show_col_types = F) %>% 
  read_csv("JASP data Attractiveness and gender influence on morality.csv") %>% select_if(function(x){!all(is.na(x))}) %>% 
  #select(-c(290,291,292,293,294,295,296,297,298)) %>% 
  mutate(pp_num=seq_along(`participant gender`)) %>% 
  dplyr::rename(pp_age = `participant age`, pp_sex=`participant gender`, morality_score=`morality judgment_SC0`) %>% 
  relocate(pp_num, pp_age, pp_sex, morality_score) %>% 
  mutate(pp_sex=str_to_lower(pp_sex)) %>% 
  mutate(pp_sex=as_factor(pp_sex)) 


dd <- dd %>% 
  rename(
  scenario_1 = `scenario 1 _1`, # notice the special character, maybe a non-breaking space
  scenario_2 = "Scenario 2_1",
  scenario_3 = "scenario 3_1",
  scenario_4 = "Scenario 4_1",
  scenario_5 = "Scenario 5_1",
  scenario_6 = "scenario 6_1",) 


scenario_data = dd %>% 
  select(pp_num, pp_age, pp_sex, morality_score, starts_with("scenario"))

attractiveness_ratings = dd %>% 
  select(pp_num, pp_age, pp_sex, morality_score, !starts_with("scenario")) %>% 
  pivot_longer(cols=-c(pp_num, pp_age, pp_sex, morality_score)) %>% 
  #
  # in name, unattractive F+M10_1 should be unattractive F10+M_1, for M_1 to M_6
  #
  mutate(
    name=ifelse(name=="unattractive F+M10_1", "unattractive F10+M_1", name),
    name=ifelse(name=="unattractive F+M10_2", "unattractive F10+M_2", name),
    name=ifelse(name=="unattractive F+M10_3", "unattractive F10+M_3", name),
    name=ifelse(name=="unattractive F+M10_4", "unattractive F10+M_4", name),
    name=ifelse(name=="unattractive F+M10_5", "unattractive F10+M_5", name),
    name=ifelse(name=="unattractive F+M10_6", "unattractive F10+M_6", name)
  )

a = attractiveness_ratings
a$stim_sex=as.character(NA) 
a <- 
  a %>% 
  # get the male / female component of the spreadsheet header and strip spaces from it
  mutate(
    stim_sex = 
      str_remove_all(
        str_extract(name, pattern="\\sfemale|\\smale|female|male|F"), 
        " ")
  ) %>% 
  # convert F to female
  mutate(stim_sex=ifelse(stim_sex=="F", "female", stim_sex)) %>% 
  mutate(stim_sex=as_factor(stim_sex))


# are they rating a face or how likely that face was to commit the act in the morality fable?
# the presence of capital M denotes morality fable response
a$rating_what = as.character(NA)
a=a %>% mutate(rating_what = ifelse(
  str_detect(name,"M"), "morality", "attractiveness"
))

# if they are rating morality, which of six fables?
a$which_morality=as.character(NA)
a=a %>% 
  mutate(which_morality = str_extract(name, pattern=regex("M_."))) %>% 
  mutate(which_morality = str_remove(which_morality, "M_"))


# extract stimulus number from name
a$stim_num = as.numeric(NA)
a = a %>% mutate(temp=str_remove(name, pattern="unattractive\\s|attractive\\s|attractive|unattractive")) 
a = a %>% separate(temp, sep="_", into=c("stem","discard")) %>% select(-discard)
# manually correct a male without a number who is deduced to be male2
a = a %>% mutate(stem=ifelse(stem=="male ", "male 2", stem)) # notice the wierd character, maybe a non-breaking space
# attractive male 15+M_* should be 5 not 15
a = a %>% mutate(stem=str_replace(stem, "15","5"))
a = a %>% mutate(stim_num=readr::parse_number(stem))

# extract attractiveness from the name
a$attr_from_name = as.character(NA)
a=
  a %>% 
  mutate(
    attr_from_name = 
      str_extract(
        str_remove_all(name, " "), 
        pattern="attractive|unattractive"))

# calculate median attractiveness per-participant for rating_what is attractiveness
a=a %>% mutate(attractiveness_rating = ifelse(rating_what=="attractiveness", value, NA))
a=
  a %>% 
  group_by(pp_num) %>% 
  mutate(median_attractiveness_for_this_pp = median(attractiveness_rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(median_attractiveness_for_this_pp = ifelse(is.na(attractiveness_rating), NA, median_attractiveness_for_this_pp))

# calculate median split and assign high or low
a=
  a %>% 
  mutate(
    median_split = 
      ifelse(attractiveness_rating > median_attractiveness_for_this_pp, "high-attractive",
             ifelse(attractiveness_rating < median_attractiveness_for_this_pp, "low-attractive",
                    ifelse(attractiveness_rating == median_attractiveness_for_this_pp, "median-attractive",NA))))

# Very many values are at the median - what should we do with these?

# morality
a=a %>% mutate(morality_rating = ifelse(rating_what=="morality", value, NA))
a=a %>% 
  group_by(pp_num, stim_num, stim_sex) %>% 
  mutate(mean_morality=mean(morality_rating, na.rm=TRUE)) %>% 
  ungroup()


m=a %>% filter(!is.na(which_morality)) %>% select(pp_num, morality_score, stim_sex,stim_num,which_morality, morality_rating, mean_morality)



# I think she said that there were 40 stimuli
# I think the attractive / unattractive in the spreadsheet header she said was her opinion
# The stim num ranges from 0 to 10, for each of 2 sexes (m/f), so that's only 20 unique stimuli
# To get 40 unique stimuli you need to incorporate Lilly's opinion of their attractive ness into the stim num 