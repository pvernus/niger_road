## AUTO_CONSUMPTION ##

auto_conso <- ehcvm_conso_agg %>% 
  group_by(grappe, hhid, modep) %>% 
  summarize(modep_depan = sum(depan))

auto_conso <- auto_conso %>%
  group_by(grappe, hhid) %>% 
  mutate(pct = modep_depan / sum(modep_depan) * 100)

achat <- auto_conso %>% 
  filter(modep == 1)

auto <- auto_conso %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  filter(modep == 2)

auto_conso <- left_join(survey_welfare %>% select(grappe, hhid), auto, by = 'hhid') %>% 
  rename(grappe = grappe.x) %>% 
  mutate(modep_depan = if_else(is.na(grappe.y), 0, modep_depan),
         auto_conso_pct = if_else(is.na(grappe.y), 0, pct)) %>% 
  select(-c(grappe.y, pct))

auto_conso <- auto_conso %>% 
  group_by(grappe) %>% 
  summarize(auto_conso = mean(auto_conso_pct))
