load(here('data','grappes.RData'))
load(here('data','short_survey.RData'))

poverty_by_grappe <- survey_welfare %>% 
  select(grappe, hhid, pcexp, zref, def_spa, def_temp) %>% 
  mutate(pcexp_def = pcexp * def_spa * def_temp,
         pov = if_else(pcexp_def < zref, 1, 0)) %>% 
  summarize(pov = sum(pov) / length(pov), .by = grappe)

save(poverty_by_grappe, file = here('data', 'poverty.RData'))