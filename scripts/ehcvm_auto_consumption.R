## AUTO_CONSUMPTION ##
load(here('data', 'short_survey.RData'))

conso <- ehcvm_conso_agg %>% 
  group_by(hhid, modep) %>% 
  summarize(modep_depan = sum(depan))

conso_pct <- conso %>%
  group_by(hhid) %>% 
  mutate(pct = modep_depan / sum(modep_depan) * 100)

achat <- conso_pct %>% 
  filter(modep == 1)

auto <- conso_pct %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  filter(modep == 2)

auto_conso <- left_join(survey_welfare %>% select(grappe, hhid), auto, by = 'hhid') %>% 
  mutate(modep_depan = if_else(is.na(modep), 0, modep_depan),
         auto_conso_pct = if_else(is.na(modep), 0, pct))

auto_conso_by_grappe <- auto_conso %>% 
  group_by(grappe) %>% 
  summarize(auto_conso = mean(auto_conso_pct))

save(auto_conso, auto_conso_by_grappe, file = here('data', 'auto_consumption.RData'))
