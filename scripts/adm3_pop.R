source("scripts/library.R") # load packages

adm3_pop <- read_csv(here("data_raw", "ner_admpop_adm3_2022.csv"))
adm3_pop <- adm3_pop %>% 
  clean_names()

# save clean data
save(adm3_pop, file = "data/adm3_pop.RData")