load("data/ner_adm.RData")
load('data/short_survey.RData')
ehcvm_menage_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_menage_ner2018.dta')

# Binomial variable at the community level. Impossible to use in the PCA.
sh_co_vio <- ehcvm_menage_ner2018 %>% 
  select(hhid, sh_co_vio) %>% 
  mutate(hhid = as_factor(hhid))

survey_welfare %>% 
  select(hhid, id_adm2, grappe) %>% 
  left_join(sh_co_vio, by = "hhid")

