# restrict to pp characteristic; attractiveness scores; morality score

a=dd %>% 
  select(
    c(
      pnum, 
      mval,
      starts_with(c("a", "u"))
      )
  ) %>% 
  select(
    -contains("+")
  ) %>% 
  pivot_longer(
    cols=starts_with(c("a", "u"))
  ) %>% 
  mutate(
    gender=str_extract(
      name,
      "male|male\\s|female|female\\s|F"
    )
  )

b=a %>%
  mutate(
    stim_num=readr::parse_number(name)
  ) %>% 
  mutate(
    au=str_extract(name, "attractive|attractive\\s|unattractive|unattractive\\s")
    ) %>% 
  mutate(stim_grp=as.numeric(factor(au))) %>% 
  arrange(pnum, gender, stim_grp, stim_num) %>% 
  # if au is unattractive then stim_num plus 10
  mutate(stim_id = ifelse(au=="unattractive", stim_num+10, stim_num)) %>% 
  mutate(stim_id=ifelse(gender=="male",stim_id+20,stim_id))

f=b %>% 
  select(pnum, mval, gender, stim_id, value) %>% 
  rename(
    participant=pnum,
    stimulus=stim_id,
    attractiveness=value,
    morality=mval) %>% 
  mutate(gender=as_factor(gender)) %>% 
  mutate(participant=paste0("p",str_pad(participant, width=2, pad=0))) %>% 
  mutate(participant=as_factor(participant)) %>% 
  mutate(stimulus=as_factor(paste0("s", str_pad(stimulus, width=2, pad="0")))) %>% 
  rename(raw_attractiveness = attractiveness)

g=f
g=g %>% 
  group_by(stimulus) %>% 
  mutate(mean_attractiveness = mean(raw_attractiveness)) %>% 
  mutate(median_attractiveness = median(raw_attractiveness)) %>% 
  mutate(
    attractiveness = 
      ifelse(mean_attractiveness < median_attractiveness, 
             "LOW_ATTRACTIVENESS", "HIGH_ATTRACTIVENESS")) %>% 
  mutate(attractiveness=as_factor(attractiveness)) %>% select(-morality) %>% 
  ungroup()

attract=g



# morality now

a2=dd %>% 
  select(
    c(
      pnum, 
      contains("M")
    )
  ) %>% 
  pivot_longer(
    cols=starts_with(c("a", "u"))
  ) %>% 
  mutate(
    gender=str_extract(
      name,
      "male|male\\s|female|female\\s|F"
    )
  ) %>% 
  mutate(gender=ifelse(gender=="F", "female", gender)) %>% 
  mutate(
    stim_num=readr::parse_number(name)
  ) %>% 
  mutate(
    au=str_extract(name, "attractive|attractive\\s|unattractive|unattractive\\s")
  ) %>% 
  mutate(stim_grp=as.numeric(factor(au))) %>% 
  arrange(pnum, gender, stim_grp, stim_num) %>% 
  # if au is unattractive then stim_num plus 10
  mutate(stim_id = ifelse(au=="unattractive", stim_num+10, stim_num)) %>% 
  mutate(stim_id=ifelse(gender=="male",stim_id+20,stim_id)) %>% 
  select(pnum, gender, stim_id, value) %>% 
  rename(
    participant=pnum,
    stimulus=stim_id,
    raw_morality=value
  ) %>% 
  mutate(gender=as_factor(gender)) %>% 
  mutate(participant=paste0("p",str_pad(participant, width=2, pad=0))) %>% 
  mutate(participant=as_factor(participant)) %>% 
  mutate(stimulus=as_factor(paste0("s", str_pad(stimulus, width=2, pad="0")))) %>% 
  group_by(participant, stimulus) %>% 
  summarise(mean_morality = mean(raw_morality, na.rm=TRUE)) %>% 
  ungroup()

moral=a2


# lilly_data=full_join(g, a2) %>% 
#   rename(morality=mean_morality) %>% 
#   select(c(participant, gender, stimulus, attractiveness, morality)) 
# #%>% pivot_wider(id_cols = c(participant), names_from=c(attractiveness, gender), values_from=morality)



both=full_join(attract, moral) %>% 
  select(participant, stimulus, gender, attractiveness, mean_morality) %>% 
  group_by(participant, gender, attractiveness) %>% 
  summarise(morality=mean(mean_morality)) %>% 
  pivot_wider(id_cols=participant, names_from=c(gender,attractiveness), values_from=morality)

write_csv(both, file="lilly_data.csv")
