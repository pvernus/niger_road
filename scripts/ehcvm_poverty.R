load(here('data','grappes.RData'))
load(here('data','short_survey.RData'))

poverty <- survey_welfare %>% 
  select(grappe, hhid, dtot, eqadu1, zref, def_spa, def_temp) %>% 
  mutate(pcexp = dtot / (eqadu1 * def_spa * def_temp),
         pov = if_else(pcexp < zref, 1, 0)) %>% 
  summarize(pov = sum(pov) / length(pov), .by = grappe)
