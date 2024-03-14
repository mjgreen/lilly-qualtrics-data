# attractiveness
a <- 
  dd %>% 
  select(c(pnum, page, psex, mval, starts_with(c("u", "a")))) %>% 
  select(-contains("+"))

a <- 
  a %>% 
  pivot_longer(
    cols=starts_with(c("a", "u")), 
    names_to="id_raw", values_to = "attrval")

a <- a %>% 
  mutate(id_proc=str_remove(id_raw, "_1"))

a <- a %>% 
  mutate(
    stim_attr = str_extract(
      id_proc,
      "attractive|attractive\\s|unattractive|unattractive\\s"
    )
  )

a <- a %>% 
  mutate(
    stim_sex = str_extract(
      id_proc,
      "male|male\\s|female|female\\s|F"
    )
  ) %>% 
  mutate(stim_sex = ifelse(
    stim_sex == "F", "female", stim_sex
  )
  )

a <- a %>% 
  mutate(
    stim_num = str_pad(readr::parse_number(id_proc), width=2, pad = "0")
  )

a <- a %>% 
  mutate(
    stim_id_full = paste(
      stim_attr, 
      stim_sex,
      stim_num,
      sep="_"
    )
  )

a <- a %>% 
  mutate(
    stim_id = paste(
      str_sub(stim_attr,1,1), 
      str_sub(stim_sex,1,1),
      str_sub(stim_num,1,2),
      sep="_"
    )
  )

a <- a %>% 
  arrange(
    pnum, stim_attr, stim_sex, stim_num, stim_id
  )  %>% 
  select(
    pnum, page, psex, mval, 
    stim_attr, stim_sex, stim_num,stim_id_full,stim_id,
    attrval
  )

a <- a %>% 
  mutate(
    stim_factor = factor(
      stim_id_full, 
      levels = sort(unique(stim_id_full)),
      labels = 1:40)) %>% 
  relocate(-attrval)
