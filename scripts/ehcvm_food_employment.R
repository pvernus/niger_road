load(here('data','short_survey.RData'))
#employment
s04_me_employment <- read_dta('data_raw/ner_2018_ehcvm/s04_me_ner2018.dta')
s04_co_employment <- read_dta('data_raw/ner_2018_ehcvm/s04_co_ner2018.dta')

### Food employment ###

# 1. Create a typology of food employment, cf. Allen et al. (2018)
food_employment <- s04_me_employment %>% 
  select(vague, grappe, menage, 
         id_ind = s01q00a, # id ind
         main_empl = s04q28a, # primary job
         month_main_empl = s04q32, # prim. job - total months worked
         day_main_empl = s04q36, # prim. job - days per month worked
         hour_main_empl = s04q37, # prim. job - hours per day worked
         starts_with("s04q29"), # prim. job - job category
         starts_with("s04q30"), # prim. job - activity category
         sec_empl = s04q28b, # secondary job
         month_sec_empl = s04q54, # sec. job - total months worked
         day_sec_empl = s04q55, # sec. job - days per month worked 
         hour_sec_empl = s04q56, # sec. job - hours per day worked 
         starts_with("s04q51"), # prim. job - job category
         starts_with("s04q52") # prim. job - activity category
  ) %>% 
  mutate(
    main_empl = as_factor(main_empl),
    across(starts_with("s04q29"), as_factor),
    across(starts_with("s04q30"), as_factor),
    across(starts_with("s04q51"), as_factor),
    across(starts_with("s04q52"), as_factor)
  ) %>% 
  rename(
    prim_code_emploi = s04q29b,
    prim_code_prof = s04q29d,
    prim_code_section = s04q30b,
    prim_branche_activite = s04q30c,
    prim_code_activite = s04q30d,
    sec_code_emploi = s04q51b,
    sec_code_prof = s04q51d,
    sec_code_section = s04q52b,    
    sec_branche_activite = s04q52c,
    sec_code_activite = s04q52d,
  ) %>% 
  mutate(prim_food_empl = as_factor(case_when(
    # food agriculture
    prim_code_prof %in% c("éleveur de bétail", "Maraîcher", "Cultivateur") ~ "Food agriculture",
    prim_code_section == "Agriculture, pêche, foresterie" ~ "Food agriculture",
    prim_branche_activite %in% c("Agriculture vivrière et activités annexes", "Agriculture industrielle et d'exportation", "Sylviculture, exploitation forestière, activités annexes", "Elevage et chasse") ~ "Food agriculture",
    
    # food processing
    prim_branche_activite == "Fabrication de produits alimentaires et de boissons" ~ "Food processing",
    # food marketing
    prim_branche_activite == "Commerce de gros et activités d'intermédiaires du commerce de gros" & prim_code_activite == "Commerce de gros de produits agricoles bruts, d'animaux vivants, de produits alimentaires, boissons et tabacs" ~ "Food marketing",
    prim_code_activite %in% c("Commerce de détail de fruits et légumes", "Commerce de détail d’autres produits alimentaires", "Commerce de détail général (alimentation, boutique, épicerie, …)") ~ "Food marketing",
    prim_branche_activite == "Transports terrestres, transport par conduites" ~ "Food marketing",
    prim_code_emploi == "AGRICULTEURS ET OUVRIERS QUALIFIES DE L'AGRICULTURE ET LA PECHE" & prim_code_activite == "Commerce de détail d’autres produits (y compris activités d’intermédiaire du commerce de détail)" ~ "Food marketing",
    # food away-from-home
    prim_branche_activite == "Hôtels et restaurants" ~ "Food away-from-home",
    .default = "Other"
  )),
  sec_food_empl = as_factor(case_when(
    # food agriculture
    sec_code_prof %in% c("ouvrier qualifié de l'agriculture", "autres agriculteurs et ouvriers qualifies de l'agriculture et la peche non classé ailleurs", "Cultivateur", "éleveur de bétail", "Maraîcher", "Ouvrier, manœuvre agricole") ~ "Food agriculture",
    sec_code_section == "Agriculture, pêche, foresterie" ~ "Food agriculture",
    sec_branche_activite %in% c("Agriculture vivrière et activités annexes", "Agriculture industrielle et d'exportation", "Sylviculture, exploitation forestière, activités annexes", "Elevage et chasse") ~ "Food agriculture",
    sec_code_emploi %in% c("éleveur de bétail", "Maraîcher", "Cultivateur") ~ "Food agriculture",
    # food processing
    sec_branche_activite == "Fabrication de produits alimentaires et de boissons" ~ "Food processing",
    # food marketing
    sec_branche_activite == "Commerce de gros et activités d'intermédiaires du commerce de gros" & sec_code_activite == "Commerce de gros de produits agricoles bruts, d'animaux vivants, de produits alimentaires, boissons et tabacs" ~ "Food marketing",
    sec_code_activite %in% c("Commerce de détail de fruits et légumes", "Commerce de détail d’autres produits alimentaires", "Commerce de détail général (alimentation, boutique, épicerie, …)") ~ "Food marketing",
    sec_branche_activite == "Transports terrestres, transport par conduites" ~ "Food marketing",
    sec_code_emploi %in% c("AGRICULTEURS ET OUVRIERS QUALIFIES DE L'AGRICULTURE ET LA PECHE", "Ouvrier, manœuvre agricole") & sec_code_activite == "Commerce de détail d’autres produits (y compris activités d’intermédiaire du commerce de détail)" ~ "Food marketing",
    sec_code_prof %in% c("vendeur de fruits", "vendeur de céréales", "vendeur de vivres frais (alloco, igname, taro, autres féculents)", "vendeur de légumes et arachides et tous condiments", "Boucher - vendeur", "Docker") ~ "Food marketing",
    # food away-from-home
    sec_branche_activite == "Hôtels et restaurants" ~ "Food away-from-home",
    sec_code_prof %in% c("vendeur de beignets et d'autres aliments préparés (vendeur d'aliments)") ~ "Food away-from-home",
    .default = "Other"
  ))
  ) %>% 
  relocate(c("prim_food_empl", "sec_food_empl"))

# 2. Convert time worked in Full-time equivalent (FTE)
food_fte <- food_employment %>% 
  right_join(survey_ind %>% select(vague, grappe, menage, id_ind = numind, hhid, sexe, age), by =c("vague", "grappe", "menage", "id_ind")) %>% 
  mutate( # create full-time equivalent variable
    fte_prim = (hour_main_empl*day_main_empl*month_main_empl)/(40*52),
    fte_sec = (hour_sec_empl*day_sec_empl*month_sec_empl)/(40*52),
    fte_agriculture = case_when(
      prim_food_empl == "Food agriculture" ~ fte_prim,
      sec_food_empl == "Food agriculture" ~ fte_sec,
      .default = 0),
    fte_processing = case_when(
      prim_food_empl == "Food processing" ~ fte_prim,
      sec_food_empl == "Food processing" ~ fte_sec,
      .default = 0),
    fte_marketing = case_when(
      prim_food_empl == "Food marketing" ~ fte_prim,
      sec_food_empl == "Food marketing" ~ fte_sec,
      .default = 0),          
    fte_away_home = case_when(
      prim_food_empl == "Food away-from-home" ~ fte_prim,
      sec_food_empl == "Food away-from-home" ~ fte_sec,
      .default = 0)
  ) %>% 
  group_by(hhid) %>% 
  summarize(
    fte_agriculture = sum(fte_agriculture, na.rm = TRUE),
    fte_processing = sum(fte_processing, na.rm = TRUE),
    fte_marketing = sum(fte_marketing, na.rm = TRUE),
    fte_away_home = sum(fte_away_home, na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(fte_total = sum(c(fte_agriculture, fte_processing, fte_marketing, fte_away_home))) %>% 
  ungroup()