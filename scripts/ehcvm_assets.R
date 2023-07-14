ehcvm_menage_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_menage_ner2018.dta')
load(here('data', 'short_survey.RData'))

asset_by_hhid <- ehcvm_menage_ner2018 %>% 
  select(hhid, frigo, cuisin, car, ordure) %>% 
  mutate(hhid = as_factor(hhid))

asset_by_grappe <- left_join(asset_by_hhid, survey_welfare %>% 
            select(hhid, grappe),
          by = 'hhid') %>% 
  summarize(
    fridge = 100 * sum(frigo) / length(frigo),
    cooking_stove = 100 * sum(cuisin) / length(cuisin),
    car = 100 * sum(car) / length(car),
    waste = 100 * sum(ordure) / length(ordure),
    .by = 'grappe'
  )

save(asset_by_grappe, file = here('data', 'asset.RData'))


