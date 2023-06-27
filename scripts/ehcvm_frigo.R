ehcvm_menage_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_menage_ner2018.dta')

ehcvm_menage_ner2018 %>% 
  select(hhid, frigo)