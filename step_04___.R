options(scipen=8)

am = full_join(
  m,
  a,
) %>% relocate(-c(mean_morality_rating))


am <- am %>% 
  relocate(pnum, page, psex, mval, stim_num, stim_id_full, stim_id, stim_factor,
           stim_attr, stim_sex,
           attrval, mean_morality_rating) 
 


# calculate median attractiveness rating 
am <- am %>% 
  ##################### GROUPING ##################
  group_by(pnum) %>% 
  ##################### GROUPING ##################
  mutate(median_attr = median(attrval)) %>% 
  ungroup()


# say for each stim whether it is hi or lo 
am <- am %>% 
  mutate(
    attractiveness_level=
      case_when(
        attrval <= median_attr ~ "LOW",
        #attrval == median_attr ~ "mid",
        attrval > median_attr ~  "HIGH",
        .default = as.character(NA)
      )
  ) 


# factorise
am=am %>% 
  mutate(
    stim_attr =    as_factor(stim_attr),
    stim_sex =     as_factor(stim_sex),
    stim_num =     as_factor(stim_num),
    stim_id_full = as_factor(stim_id_full),
    stim_id =      as_factor(stim_id),
    attractiveness_level = as_factor(attractiveness_level)
  )

# reduce
am.reduced = am %>% 
  group_by(pnum, stim_sex, attractiveness_level) %>% 
  summarise(mean_morality_rating=mean(mean_morality_rating),.groups = 'keep') %>% 
  ungroup() 

# The reduced data are incomplete ...
#print(with(am.reduced, table(pnum, stim_sex, attractiveness_level))) 

# ... because subject 12 had no HIGH males
filter(am.reduced, pnum==12, stim_sex=="male") %>% pull(attractiveness_level) %>% unique()

# remove pnum 12 for not having any HIGH males
am <- am %>% filter(pnum != 12)
#reduce again after removing
am.reduced = am %>% 
  group_by(pnum, stim_sex, attractiveness_level) %>% 
  summarise(mean_morality_rating=mean(mean_morality_rating),.groups = 'keep') %>% 
  ungroup() 

# the data are balanced after removing pnum 12
#print(with(am.reduced, table(pnum, stim_sex, attractiveness_level))) 


###   MEANS   ####

am.reduced.means <- am.reduced %>% 
  group_by(stim_sex, attractiveness_level) %>% 
  summarise(mean_morality_rating=mean(mean_morality_rating))

plot1=ggplot(data=am %>% mutate(attractiveness_level=factor(attractiveness_level, levels=c("LOW", "HIGH"))),
       aes(y=mean_morality_rating, 
           group=stim_sex,
           x=attractiveness_level,
           color=stim_sex,
           shape=stim_sex)
       ) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  stat_summary(position = position_dodge(width=0.2), size=1)+
  stat_summary(fun.y=mean, aes(colour=stim_sex), geom="line", position = position_dodge(width=0.2))+
  ylab("Degree of blame in scenarios")+
  xlab("Attractiveness vs participant-level median")+
  labs(color="gender",shape="gender")

print(plot1)

library(ez)

message("\nmodel3, unreduced\n")
am$pnum = as_factor(am$pnum)
model3=
  ezANOVA(am, 
          dv=mean_morality_rating,
          wid=pnum,
          within=c(stim_sex, attractiveness_level),
          return_aov = F
  )
print(model3)

message("\nmodel4\n")
am.reduced$pnum = as_factor(am.reduced$pnum)
model4=
  ezANOVA(am.reduced, 
          dv=mean_morality_rating,
          wid=pnum,
          within=c(stim_sex, attractiveness_level),
          return_aov = F
  )
print(model4)

# print(model3$ANOVA)
# #print(model3$aov)
# library(report)
# report(model3$aov)
