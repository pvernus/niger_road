
load(here('data', 'short_survey.RData'))

s08a_me_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/s08a_me_ner2018.dta') %>% 
  select(grappe, menage, s08aq02, s08aq03)

diet_by_hhid <- left_join(s08a_me_ner2018,
                          survey_welfare %>% select(grappe, menage, hhid),
                          by = c('grappe', 'menage'),
                          keep = NULL) %>% 
  mutate(s08aq02 = if_else(s08aq02 > 1, 0, s08aq02),
         s08aq03 = if_else(s08aq03 > 1, 0, s08aq03)
  )

diet_by_grappe <- diet_by_hhid %>% 
  summarize(
    healthy_diet = 100 * sum(s08aq02) / length(s08aq02),
    varied_diet =  100 * sum(s08aq03) / length(s08aq03),
    .by = 'grappe'
  )

save(diet_by_grappe, file = here('data', 'diet.RData'))
