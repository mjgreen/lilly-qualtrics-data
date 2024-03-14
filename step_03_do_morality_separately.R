# morality
m <-
  dd %>% 
  select(
    c(pnum, page, psex, mval, contains("+")))

m = m %>% 
  pivot_longer(
    cols=contains("+"),
    names_to="raw_name",
    values_to="morval"
  )

m=m %>% 
  mutate(
    stim_attr = str_extract(
      raw_name,
      "attractive|attractive\\s|unattractive|unattractive\\s"
    )
  )

m=m %>% 
  mutate(
    stim_sex = str_extract(
      raw_name,
      "male|male\\s|female|female\\s|F"
    )
  ) %>% 
  mutate(stim_sex = ifelse(
    stim_sex == "F", "female", stim_sex))

m=m %>% 
  mutate(
    stim_num = str_pad(readr::parse_number(raw_name), width=2, pad = "0")
  )
# manually correct 15
m=m %>% 
  mutate(
    stim_num = ifelse(stim_num=="15", "05", stim_num)
  )

m=m %>% 
  mutate(
    which_moral = str_extract(raw_name, "M..")
  )
      
  
m=m %>% 
  mutate(
    stim_id_full = paste(
      stim_attr, 
      stim_sex,
      stim_num,
      sep="_"
    )
  )

m=m %>% 
  mutate(
    stim_id = paste(
      str_sub(stim_attr,1,1), 
      str_sub(stim_sex,1,1),
      str_sub(stim_num,1,2),
      sep="_"
    )
  )

m=m %>% 
  arrange(
    pnum, stim_attr, stim_sex, stim_num, stim_id
  ) 

m=m%>% 
  select(
    pnum, page, psex, mval, 
    stim_attr, stim_sex, stim_num, stim_id_full, stim_id,
    morval,
    everything()
  )

m=m %>% 
  mutate(
    stim_factor = factor(
      stim_id_full, 
      levels = sort(unique(stim_id_full)),
      labels = 1:40)) %>% 
  relocate(-morval) %>% 
  select(-raw_name)

m_full=m
#view(m_full)

# average morval by pp, collapsing the different fables M_1 to M_6
m=m %>% 
  group_by(pnum, page, psex, mval, 
           stim_attr, stim_sex, stim_num, stim_id_full, stim_id, stim_factor) %>%
  summarise(mean_morality_rating = mean(morval), 
            .groups="drop_last") %>% 
  ungroup()



