load(here('data','short_survey.RData'))
load(here('data','grappes.RData'))

s16a_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s16a_me_ner2018.dta') # Section 16: Agriculture

### Households' food production ###

s16a_data <- s16a_me_agric %>% 
  select(vague, grappe, menage, 
         champ = s16aq02, 
         nb_culture = s16aq07, 
         princ_culture = s16aq08, 
         s16aq09a,
         mode_occup = s16aq10,
         unit_superf = s16aq09b
         ) %>% 
  mutate(across(c(princ_culture, mode_occup), as_factor),
         unit_superf = if_else(s16aq09a > 4000, 2, unit_superf), # treatment of outliers
         superficie_ha = if_else(unit_superf == 2, s16aq09a/10000, s16aq09a),
         superficie_ha = if_else(is.na(superficie_ha), 0, superficie_ha) # convert sqrm to ha
         )

hh_food_prod <- left_join(survey_welfare %>% 
                            select(grappe, menage, hhid, milieu), 
                          s16a_data,
                          by = c('grappe', 'menage')) %>%
  mutate(grappe = as_factor(grappe))

food_prod <- hh_food_prod %>% 
  mutate(hh_total_superf = sum(superficie_ha, na.rm = TRUE), # total cultivated area per HH
         landless = if_else(hh_total_superf < 1 | mode_occup != 'Propriétaire', 1, 0), 
         .by = hhid) # landless HH (i) < 1 ha of total cultivated land OR (ii) do not own any cultivated land

# merge with livestock 
load(here('data', 'livestock.RData'))

join_large_livestock_landless_by_grappe <- livestock %>% 
  select(grappe, hhid, large_livestock) %>% 
  group_by(grappe, hhid) %>% 
  summarize(large_livestock = max(large_livestock)) %>% 
  ungroup() %>% 
  left_join(food_prod %>% 
              select(hhid, landless) %>% 
              summarize(landless = max(landless), .by = "hhid"))

agri_activity_by_hhid <- join_large_livestock_landless_by_grappe %>%
  mutate(
    cropping_only = if_else(large_livestock == 0 & landless == 0, 1, 0),
    landless_only = if_else(large_livestock == 0 & landless == 1, 1, 0),
    cropping_pastoral = if_else(large_livestock == 1 & landless == 0, 1, 0),
    landless_pastoral = if_else(large_livestock == 1 & landless == 1, 1, 0),
    agri_activity = as_factor(case_when(
      large_livestock == 0 & landless == 0 ~ 'cropping_only',
      large_livestock == 0 & landless == 1 ~ 'landless_only',
      large_livestock == 1 & landless == 0 ~ 'cropping_pastoral',
      large_livestock == 1 & landless == 1 ~ 'landless_pastoral'
    ))
  )

pivot_longer_agri_activity_by_grappe <- agri_activity_by_hhid %>% 
  pivot_longer(
    cols = cropping_only:landless_pastoral,
    names_to = 'agri_activity_type',
    values_to = 'value'
  ) %>% 
  summarize(
    value = sum(value),
    .by = c(grappe, agri_activity_type)
  ) %>% 
  mutate(
    agri_activity = 100 * value / sum(value),
    .by = grappe
  )

agri_activity_by_grappe <- pivot_longer_agri_activity_by_grappe %>% 
  filter(value > 0) %>% 
  pivot_wider(names_from = agri_activity_type,
              values_from = agri_activity) %>% 
  summarize(
    landless_only = sum(landless_only, na.rm = TRUE),
    landless_pastoral = sum(landless_pastoral, na.rm = TRUE),
    cropping_only = sum(cropping_only, na.rm = TRUE),
    cropping_pastoral = sum(cropping_pastoral, na.rm = TRUE),
    pastoral = landless_pastoral + cropping_pastoral,
    landless = landless_only + landless_pastoral,
    .by = 'grappe'
  )

save(agri_activity_by_grappe, file = here('data', 'agri_activity.RData'))




load(here('data','conso_survey.RData'))

t_test <- left_join(agri_activity_hhid %>% 
            select(grappe, hhid, agri_activity), 
          fcons %>% 
            select(grappe, hhid, id_adm1, milieu, fcons_pc), 
          by = 'hhid')

t.test(t_test[t_test$agri_activity=='cropping_only', 7], t_test[t_test$agri_activity=='landless_only', 7])
t.test(t_test[t_test$agri_activity=='cropping_only', 7], t_test[t_test$agri_activity=='cropping_pastoral', 7]) # p-value = 0.5465
t.test(t_test[t_test$agri_activity=='cropping_only', 7], t_test[t_test$agri_activity=='landless_pastoral', 7])

t.test(t_test[t_test$agri_activity=='landless_only', 7], t_test[t_test$agri_activity=='cropping_pastoral', 7])
t.test(t_test[t_test$agri_activity=='landless_only', 7], t_test[t_test$agri_activity=='landless_pastoral', 7])

t.test(t_test[t_test$agri_activity=='cropping_pastoral', 7], t_test[t_test$agri_activity=='landless_pastoral', 7])


