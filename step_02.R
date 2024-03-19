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
  mutate(median_attractiveness = median(raw_attractiveness)) %>% 
  mutate(
    attractiveness = 
      ifelse(raw_attractiveness < median_attractiveness, 
             "LOW_ATTRACTIVENESS", "HIGH_ATTRACTIVENESS")) %>% 
  mutate(attractiveness=as_factor(attractiveness)) 
  

h=g
h=h %>% 
  select(participant, gender, attractiveness, morality)
write_csv(h, "lillydata.csv")
