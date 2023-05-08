source('scripts/library.R') # load packages
source('R/functions.R') # load functions
# load('data/short_survey.RData')
# load('data/conso_survey.RData')

url <- 'https://microdata.worldbank.org/index.php/catalog/4296/download/56136'
download.file(url, 'data_raw/ner_2018_ehcvm.zip', mode='wb')
# unzip('data_raw/ner_2018_ehcvm.zip')

# pre-processed
ehcvm_individu_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_individu_ner2018.dta')
ehcvm_menage_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_menage_ner2018.dta')
ehcvm_welfare_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_welfare_ner2018.dta')
grappe_gps_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/grappe_gps_ner2018.dta')
#employment
s04_me_employment <- read_dta('data_raw/ner_2018_ehcvm/s04_me_ner2018.dta')
s04_co_emplyment <- read_dta('data_raw/ner_2018_ehcvm/s04_co_ner2018.dta')
#food consumption
ehcvm_conso_agg <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_conso_ner2018.dta')
s07b_me_food_cons <- read_dta('data_raw/ner_2018_ehcvm/s07b_me_ner2018.dta')
#food security
s08a_me_food_sec <- read_dta('data_raw/ner_2018_ehcvm/s08a_me_ner2018.dta')
s08b1_me_food_sec <- read_dta('data_raw/ner_2018_ehcvm/s08b1_me_ner2018.dta')
s08b2_me_fodd_sec <- read_dta('data_raw/ner_2018_ehcvm/s08b2_me_ner2018.dta')
#non-agricultural enterprises
s10_1_me_nagric_ent <- read_dta('data_raw/ner_2018_ehcvm/s10_1_me_ner2018.dta')
s10_2_me_nagric_ent <- read_dta('data_raw/ner_2018_ehcvm/s10_2_me_ner2018.dta')
# agric
s16a_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s16a_me_ner2018.dta')
s16c_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s16c_me_ner2018.dta')


# add primary/foreign keys to tables
survey_ind <- ehcvm_individu_ner2018 %>% 
  mutate(id_adm1 = paste0(country, '00', region),
         id_adm2 = case_when(
           sousregion == 11 ~ 'NER001001',
           sousregion == 12 ~ 'NER001002',
           sousregion == 13 ~ 'NER001003',
           sousregion == 14 ~ 'NER001004',
           sousregion == 15 ~ 'NER001005',
           sousregion == 16 ~ 'NER001006',
           sousregion == 21 ~ 'NER002001',
           sousregion == 22 ~ 'NER002002',
           sousregion == 23 ~ 'NER002003',
           sousregion == 24 ~ 'NER002004',
           sousregion == 25 ~ 'NER002005',
           sousregion == 26 ~ 'NER002006',
           sousregion == 31 ~ 'NER003001',
           sousregion == 32 ~ 'NER003002',
           sousregion == 33 ~ 'NER003003',
           sousregion == 34 ~ 'NER003004',
           sousregion == 35 ~ 'NER003005',
           sousregion == 36 ~ 'NER003006',
           sousregion == 37 ~ 'NER003007',
           sousregion == 38 ~ 'NER003008',
           sousregion == 41 ~ 'NER004001',
           sousregion == 42 ~ 'NER004002',
           sousregion == 43 ~ 'NER004003',
           sousregion == 44 ~ 'NER004004',
           sousregion == 45 ~ 'NER004005',
           sousregion == 46 ~ 'NER004006',
           sousregion == 47 ~ 'NER004008',
           sousregion == 48 ~ 'NER004009',
           sousregion == 51 ~ 'NER005001',
           sousregion == 52 ~ 'NER005002',
           sousregion == 53 ~ 'NER005003',
           sousregion == 54 ~ 'NER005004',
           sousregion == 55 ~ 'NER005005',
           sousregion == 56 ~ 'NER005006',
           sousregion == 57 ~ 'NER005007',
           sousregion == 58 ~ 'NER005008',
           sousregion == 59 ~ 'NER005009',
           sousregion == 61 ~ 'NER006001',
           sousregion == 62 ~ 'NER006002',
           sousregion == 63 ~ 'NER006003',
           sousregion == 64 ~ 'NER006004',
           sousregion == 65 ~ 'NER006005',
           sousregion == 66 ~ 'NER006006',
           sousregion == 67 ~ 'NER006007',
           sousregion == 68 ~ 'NER006008',
           sousregion == 69 ~ 'NER006009',
           sousregion == 71 ~ 'NER007001',
           sousregion == 72 ~ 'NER007002',
           sousregion == 73 ~ 'NER007003',
           sousregion == 74 ~ 'NER007004',
           sousregion == 75 ~ 'NER007005',
           sousregion == 76 ~ 'NER007006',
           sousregion == 77 ~ 'NER007007',
           sousregion == 78 ~ 'NER007008',
           sousregion == 79 ~ 'NER007009',
           sousregion == 490 ~ 'NER004007',
           sousregion == 510 ~ 'NER005010',
           sousregion == 511 ~ 'NER005011',
           sousregion == 512 ~ 'NER005012',
           sousregion == 590 ~ 'NER005013',
           sousregion == 610 ~ 'NER006010',
           sousregion == 611 ~ 'NER006011',
           sousregion == 612 ~ 'NER006012',
           sousregion == 613 ~ 'NER006013',
           sousregion == 710 ~ 'NER007010',
           sousregion == 790 ~ 'NER007011',
           sousregion == 890 ~ 'NER008001',
           TRUE ~ as.character(NA)
           ),
         hhid = as_factor(hhid)) %>% 
  relocate(starts_with('id_adm'), .after = sousregion)

# N.B. Five departements don't have any obs. in the HH survey data: Banibangou, Tillia, Bosso,Tassara, and Bilma.
# departements %>% anti_join(survey_ind, by = 'id_adm2')

# survey_menage
survey_menage <- ehcvm_menage_ner2018 %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  left_join(survey_ind %>% select(hhid, id_adm1, id_adm2, hhweight) %>% distinct(), by = 'hhid', copy = TRUE) %>% 
  relocate(c(starts_with('id_adm'), hhweight), .after = hhid)

# survey_welfare
survey_welfare <- ehcvm_welfare_ner2018 %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  left_join(survey_ind %>% select(hhid, id_adm1, id_adm2) %>% distinct(), by = 'hhid', copy = TRUE) %>% 
  relocate(starts_with('id_adm'), .after = hhid)

# food_security
# n.b. there are three tables in Section 8: Food security

add_id_and_wght <- function(df) { # new function which adds identifier var. (hhid, id_adm*) and weight (hhweight) to tables 
  
x <- df %>% 
  mutate(hhid = as_factor( 
    if_else(menage < 10,
            str_insert(paste0(grappe, 0, menage), -1, '0'), # ad hoc function, see functions.R
            paste0(grappe, 0, menage))
    )) %>% 
    left_join(survey_ind %>% select(hhid, id_adm1, id_adm2, hhweight) %>% distinct(), by = 'hhid', copy = TRUE) %>% 
    relocate(c(hhid, starts_with('id_adm'), hhweight), before = vague)

return(x)

}

food_security_a <- add_id_and_wght(s08a_me_food_sec)
food_security_b1 <- add_id_and_wght(s08b1_me_food_sec)
food_security_b2 <- add_id_and_wght(s08b2_me_fodd_sec)

# specifying the survey sample design
survey_desgn <- svydesign(id = ~ grappe+hhid, 
          strata = ~ region+zae, 
          weights = ~ hhweight, 
          data = survey_welfare, nest = TRUE)

# save data
save(ehcvm_individu_ner2018, ehcvm_menage_ner2018, ehcvm_welfare_ner2018, grappe_gps_ner2018, # pre-processed
     s04_me_employment, s04_co_emplyment, # employment
     ehcvm_conso_agg, s07b_me_food_cons, # food consumption
     s08a_me_food_sec, s08b1_me_food_sec, s08b2_me_fodd_sec, # food security
     s10_1_me_nagric_ent, s10_2_me_nagric_ent, # non-agricultural enterprises
     survey_ind, survey_menage, survey_welfare, survey_desgn, # clean/processed data
     food_security_a, food_security_b1, food_security_b2, # processed food security data
     file = 'data/full_survey.RData')

save(survey_ind, survey_menage, survey_welfare, survey_desgn, # clean/processed data
     food_security_a, food_security_b1, food_security_b2, # processed food security data
     file = 'data/short_survey.RData')


## Level of food consumption per capita + share of food consumption in total consumption

fcons <- survey_welfare %>% 
  mutate(
    fcons_pc = dali/(eqadu1*def_spa*def_temp),
    sh_fcons = (dali/dtot)*100
  ) %>% 
  select(hhid, grappe, region, zae, id_adm1, id_adm2, milieu, hhweight, fcons_pc, sh_fcons)

fcons_w <- fcons %>%
  as_survey_design(ids = c(grappe,hhid), 
                   strata = c(region, zae), 
                   weights = hhweight,
                   variables = c(hhid, id_adm1, id_adm2, zae, milieu, fcons_pc, sh_fcons),
                   nest = TRUE)

fcons_w %>% 
  group_by(interact(id_adm2, milieu)) %>% 
  summarise(mean_fcons_pc = survey_mean(fcons_pc),
            mean_sh_fcons = survey_mean(sh_fcons),
            median_fcons_pc = survey_median(fcons_pc),
            median_sh_fcons = survey_median(sh_fcons)) %>% 
  arrange(desc(mean_fcons_pc))

fcons_w %>% 
  group_by(interact(id_adm1, milieu)) %>% 
  summarise(pcfood_cons = survey_quantile(fcons_pc, c(0.1, 0.9))
  ) %>% 
  mutate(ineq_pcfood_cons = pcfood_cons_q90/pcfood_cons_q10) %>% 
  arrange(desc(pcfood_cons_q90))


## Food basket
conso_agg <- ehcvm_conso_agg %>% 
  select(!c(country, year)) %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  left_join(survey_welfare %>% select(hhid, id_adm1, id_adm2, zae, dali, dnal, dtot, def_spa, def_temp), by = 'hhid')

# depan != dtot; does not seem to be related to modep
conso_agg %>% 
  group_by(hhid) %>% 
  reframe(depan_tot = sum(depan), 
            dtot = dtot) %>% 
  distinct(hhid, depan_tot, dtot)

conso_agg <- conso_agg %>% 
  mutate(depan_def = depan/(def_spa*def_temp)) %>% 
  filter(codpr %in% 1:128) %>% # filter food items
  mutate(food_cat = case_when(
    codpr %in% 1:22 ~ 'Céréales et produits céréaliers',
    codpr %in% 23:43 ~ 'Poisson, fruits de mer et, viande',
    codpr %in% 44:52 ~ 'Lait et produits laitiers, oeufs',
    codpr %in% 53:59 ~ 'Huile et graisse',
    codpr %in% 60:71 ~ 'Fruits',
    codpr %in% 72:91 ~ 'Légumes',
    codpr %in% 92:103 ~ 'Légumineuses et graines',
    codpr %in% 104:113 ~ 'Tubercules et plantains ',
    codpr %in% 114:117 ~ 'Sucre',
    codpr %in% 118:127 ~ 'Epices et condiments',
    codpr == 128 ~ 'Autres produits alimentaires',
    TRUE ~ as.character(NA)
  ),
  food_item = case_when(
    codpr %in% c(1:2) ~ 'Riz local (type 1 et 2)',
    codpr %in% c(3:4) ~ 'Riz importé (type 1 et 2)',
    codpr %in% c(5:6) ~ 'Maïs (épi/grain)',
    codpr == 7 ~ 'Mil',
    codpr == 8 ~ 'Sorgho',
    codpr == 9 ~ 'Blé',
    codpr == 10 ~ 'Fonio',
    codpr == 11 ~ 'Autres céréales',
    codpr == 12 ~ 'Farine de maïs',
    codpr == 13 ~ 'Farine de mil',
    codpr == 14 ~ 'Farine de blé local ou importé',
    codpr == 15 ~ 'Autres farines de céréales',
    codpr == 16 ~ 'Pâtes alimentaires',
    codpr == 17 ~ 'Pain moderne',
    codpr == 18 ~ 'Pain traditionnel',
    codpr == 19 ~ 'Croissants',
    codpr == 20 ~ 'Biscuits',
    codpr == 21 ~ 'Gâteaux',
    codpr == 22 ~ 'Beignets, galettes',
    codpr == 23 ~ 'Viande de bœuf',
    codpr == 24 ~ 'Viande de chameau',
    codpr == 25 ~ 'Viande de mouton',
    codpr == 26 ~ 'Viande de chèvre',
    codpr == 27 ~ 'Abats et tripes (foie, rognon, etc.)',
    codpr == 28 ~ 'Viande de porc',
    codpr == 29 ~ 'Poulet sur pied',
    codpr == 30 ~ 'Viande de poulet',
    codpr == 31 ~ 'Viande autres volailles domestiques',
    codpr == 32 ~ 'Charcuterie (jambon, saucisson), conserves de viandes',
    codpr == 33 ~ 'Gibiers',
    codpr == 34 ~ 'Autres viandes n.d.a.',
    codpr %in% c(35:38) ~ 'Poisson frais',
    codpr %in% c(39:40) ~ 'Poisson fumé',
    codpr == 41 ~ 'Poisson séché',
    codpr == 42 ~ 'Crabes, crevettes et autres fruits de mer',
    codpr == 43 ~ 'Conserves de poisson',
    codpr == 44 ~ 'Lait frais',
    codpr == 45 ~ 'Lait caillé, yaourt',
    codpr %in% c(46:47) ~ 'Lait concentré (non-)sucré',
    codpr == 48 ~ 'Lait en poudre',
    codpr == 49 ~ 'Fromage',
    codpr == 50 ~ 'Lait et farines pour bébé',
    codpr == 51 ~ 'Autres produits laitiers',
    codpr == 52 ~ 'Œufs',
    codpr == 53 ~ 'Beurre',
    codpr == 54 ~ 'Beurre de karité',
    codpr == 55 ~ 'Huile de palme rouge',
    codpr == 56 ~ 'Huile arachide',
    codpr == 57 ~ 'Huile de coton',
    codpr == 58 ~ 'Huile de palme raffinée',
    codpr == 59 ~ 'Autres huiles n.d.a. (maïs, soja, huile palmiste, etc.)',
    codpr == 60 ~ 'Mangue',
    codpr == 61 ~ 'Ananas',
    codpr == 62 ~ 'Orange',
    codpr == 63 ~ 'Banane douce',
    codpr == 64 ~ 'Citrons',
    codpr == 65 ~ 'Autres agrumes',
    codpr == 66 ~ 'Avocats',
    codpr == 67 ~ 'Pastèque, Melon',
    codpr == 68 ~ 'Dattes',
    codpr == 69 ~ 'Noix de coco',
    codpr == 70 ~ 'Canne à sucre',
    codpr == 71 ~ 'Autres fruits (pommes, raisin, etc.)',
    codpr == 72 ~ 'Salade (laitue)',
    codpr == 73 ~ 'Choux',
    codpr == 74 ~ 'Carotte',
    codpr == 75 ~ 'Haricot vert',
    codpr == 76 ~ 'Concombre',
    codpr == 77 ~ 'Aubergine, Courge/Courgette',
    codpr == 78 ~ 'Poivron frais',
    codpr == 79 ~ 'Tomate fraîche',
    codpr == 80 ~ 'Tomate séchée',
    codpr == 81 ~ 'Gombo frais',
    codpr == 82 ~ 'Gombo sec',
    codpr == 83 ~ 'Oignon frais',
    codpr == 84 ~ 'Ail',
    codpr %in% c(85:88) ~ 'Feuilles locales',
    codpr == 89 ~ 'Autres légumes en feuilles',
    codpr == 90 ~ 'Autre légumes frais n.d.a.',
    codpr == 91 ~ 'Concentré de tomate',
    codpr == 92 ~ 'Petits pois',
    codpr == 93 ~ 'Petit pois secs',
    codpr == 94 ~ 'Autres légumes secs n.d.a.',
    codpr == 95 ~ 'Niébé/Haricots secs',
    codpr == 96 ~ 'Arachides fraîches en coques',
    codpr == 97 ~ 'Arachides séchées en coques',
    codpr == 98 ~ 'Arachides décortiquées ou pilées',
    codpr == 99 ~ 'Arachide grillée',
    codpr == 100 ~ 'Pâte arachide',
    codpr == 101 ~ 'Sésame',
    codpr == 102 ~ 'Noix de cajou',
    codpr == 103 ~ 'Noix de karité',
    codpr == 104 ~ 'Manioc',
    codpr == 105 ~ 'Igname',
    codpr == 106 ~ 'Plantain',
    codpr == 107 ~ 'Pomme de terre',
    codpr == 108 ~ 'Taro, macabo',
    codpr == 109 ~ 'Patate douce',
    codpr == 110 ~ 'Autres tubercules n.d.a.',
    codpr == 111 ~ 'Farines de manioc',
    codpr == 112 ~ 'Gari, tapioca',
    codpr == 113 ~ 'Attiéke',
    codpr == 114 ~ 'Sucre (poudre ou morceaux)',
    codpr == 115 ~ 'Miel',
    codpr == 116 ~ 'Chocolat à croquer, pâte à tartiner',
    codpr == 117 ~ 'Caramel, bonbons, confiseries, etc.',
    codpr == 118 ~ 'Sel',
    codpr == 119 ~ 'Piment',
    codpr == 120 ~ 'Gingembre',
    codpr == 121 ~ 'Cube alimentaire (Maggi, Jumbo, etc.)',
    codpr == 122 ~ 'Arôme (Maggi, Jumbo, etc.)',
    codpr == 123 ~ 'Soumbala (moutarde africaine)',
    codpr == 124 ~ 'Mayonnaise',
    codpr == 125 ~ 'Vinaigre /moutarde',
    codpr == 126 ~ 'Autres condiments (poivre etc.)',
    codpr == 127 ~ 'Noix de cola',
    codpr == 128 ~ 'Autres produits alimentaires',
    TRUE ~ as.character(NA)
  ),
  region = as_factor(region),
  milieu = as_factor(milieu),
  modep = as_factor(modep)
    )

conso_agg_w <- conso_agg %>%
  as_survey_design(ids = c(grappe,hhid), 
                   strata = c(region, zae), 
                   weights = hhweight,
                   nest = TRUE)

save(fcons, fcons_w, conso_agg, conso_agg_w, file = 'data/conso_survey.RData')


# Plots

conso_agg_w %>% 
  group_by(adm_01, food_item, food_cat) %>% 
  summarise(total = survey_total(depan_def, na.rm = TRUE)) %>% 
  ungroup() %>% 
  slice_max(total, n = 10, by = adm_01) %>% 
  mutate(adm_01 = as.factor(adm_01),
         food_item = reorder_within(food_item, total, adm_01)) %>%
  ggplot(aes(x = food_item, y = total, fill = food_cat)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~adm_01, scales = "free_y") +
  labs(x = 'Produits alimentaires', y = 'Dépenses annuelles', fill = 'Catégorie') +
  theme_bw() +
  theme(legend.position="bottom",
        legend.text = element_text(size=10))

ggsave("outputs/plot_food_basket_region.png", width = 40, height = 20, units = "cm")

# by milieu and modep
conso_agg_w %>% 
  group_by(milieu, modep, food_item, food_cat) %>% 
  summarise(total = survey_total(depan_def, na.rm = TRUE)) %>% 
  ungroup() %>% 
  slice_max(total, n = 10, by = c(milieu, modep)) %>% 
  mutate(food_item = tidytext::reorder_within(food_item, total, list(milieu, modep))) %>%
  ggplot(aes(x = food_item, y = total, fill = food_cat)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~milieu+modep, scales = "free_y") +
  labs(x = 'Produits alimentaires', y = 'Dépenses annuelles', fill = 'Catégorie') +
  theme_bw() +
  theme(legend.position="bottom",
        legend.text = element_text(size=10))

ggsave("outputs/plot_food_basket_milieu_modep.png", width = 40, height = 20, units = "cm")

# by zae
conso_agg %>% 
  group_by(zae, food_item, food_cat) %>% 
  summarize(total = sum(depan_def)) %>% 
  ungroup() %>% 
  slice_max(total, n = 10, by = zae) %>% 
  mutate(zae = as.factor(zae),
         food_item = reorder_within(food_item, total, zae)) %>%
  ggplot(aes(x = food_item, y = total, fill = food_cat)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~id_adm1, scales = "free_y") +
  theme_bw()

# Agriculture

agric_a <- s16a_me_agric %>% 
  select(vague, grappe, menage, s16aq02, s16aq03, s16aq07, s16aq08, s16aq09a, s16aq09b) %>% 
  rename(champ = s16aq02,
         parcelle = s16aq03,
         nb_culture = s16aq07,
         princ_culture = s16aq08,
         unit_superf = s16aq09b) %>% 
  mutate(princ_culture = as_factor(princ_culture),
         unit_superf = if_else(s16aq09a > 4000, 2, unit_superf), # outliers
         superficie = if_else(unit_superf == 2, s16aq09a/10000, s16aq09a) # convert sqrm to hect
         ) %>% 
  filter(!princ_culture == 'NA') %>% 
  left_join(survey_welfare %>% select(hhid, id_adm1, id_adm2, vague, grappe, menage, zae, region, milieu, hhweight, hhsize), by = c("vague", "grappe", "menage")) %>% 
  mutate(zae = as_factor(zae),
         region = str_to_title(as_factor(region)),
         milieu = str_to_title(as_factor(milieu)))

# Superficie culture principale par region
p <- agric_a %>% 
  group_by(region, princ_culture) %>% 
  summarize(superficie = sum(superficie, na.rm = TRUE)) %>% 
  arrange(desc(superficie))

p %>% mutate(princ_culture = reorder_within(princ_culture, superficie, region)) %>%
  ggplot(aes(x = princ_culture, y = superficie)) +
  geom_col(fill = '#2d67cc', show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~region, scales = 'free_y', nrow = 2) +
  labs(x = 'Culture principale par parcelle', y = 'Superficie (Ha)') +
  theme_minimal() +
  theme(axis.text = element_text(size=12),
        strip.text = element_text(size=14, face = "bold"))

ggsave("outputs/plot_main_crop_area.png", width = 40, height = 20, units = "cm")

agric_c <- s16c_me_agric %>% 
  select(vague, grappe, menage, s16cq04, s16cq08, s16cq18, s16cq19, s16cq20, s16cq23,
         s16cq24, s16cq25, s16cq28, starts_with('s16cq29')) %>% 
  mutate(s16cq04 = as_factor(s16cq04),
         s16cq19 = as_factor(s16cq19),
         s16cq28 = as_factor(s16cq28)) %>% 
  rename(princ_culture = 's16cq04',
         buyer = 's16cq19') %>% 
  mutate_at(vars(matches("s16cq29")), as_factor) %>% 
  pivot_longer(starts_with('s16cq29'), names_to = 'princ_diff', values_to = 'rep') %>% 
  mutate(princ_diff = fct_recode(princ_diff, 
              'Autre' = "s16cq29__7", 
              'Marché - Demande' = "s16cq29__5", 'Marché - Prix' = "s16cq29__6",
              'Transport - Coût' = "s16cq29__3", 'Transport - Qualité des routes' = "s16cq29__4", 
              'Distance - Route' = "s16cq29__1", 'Distance - Marché' = "s16cq29__2"
              ),
         rep = fct_relevel(rep, 'Non, pas choisi', after = Inf)) %>% 
  left_join(survey_welfare %>% select(hhid, id_adm1, id_adm2, vague, grappe, menage, zae, region, milieu, hhweight, hhsize), by = c("vague", "grappe", "menage")) %>% 
  mutate(zae = as_factor(zae),
         region = str_to_title(as_factor(region)),
         milieu = str_to_title(as_factor(milieu)))

palette <- c('#2d67cc','#e4e6eb')

agric_c %>% 
  group_by(region, s16cq28) %>% 
  summarize(count = n()) %>% 
  mutate(perc = count/sum(count)*100) %>% 
  rename(difficulte_vente = s16cq28) %>% 
  mutate(region = reorder(region, perc)) %>% 
  ggplot(aes(fill = difficulte_vente, x = region, y = perc, label = perc)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=palette) +
  labs(x = "", y = "Pourcentage", fill = "Difficulté à écouler la production") +
  ggtitle("Les difficultés à écouler la production touchent\nen premier lieu la région d'Agadez") +
  theme_minimal() +
  theme(axis.text = element_text(size=16),
        legend.text = element_text(size=14),
        title = element_text(size=19, face = 'bold'),
        legend.position="bottom")

ggsave("outputs/plot_diff_to_sell.png", width = 22, height = 30, units = "cm")

palette <- c('#2d67cc','#6f9be8','#e4e6eb')

agric_c %>%
  filter(s16cq28 == 'Oui') %>% 
  group_by(region, princ_diff, rep) %>% 
  tally() %>% 
  ggplot(aes(fill = rep, x = princ_diff, y = n)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values=palette) +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  facet_wrap(~region) +
  labs(x = '', y = '', fill = '', 
       legend = "Principales difficultés dans l'écoulement du produit") +
  ggtitle("La faiblesse des prix Hors Champ limite l'offre,\nles contraintes logistiques varient selon les régions") +
  theme_minimal_grid() +
  theme(strip.text = element_text(size=14, face = "bold"),
        legend.text = element_text(size=16), legend.position="bottom",
        title = element_text(size=16, face = "bold"))

ggsave("outputs/plot_reason_diff_to_sell.png", width = 30, height = 20, units = "cm")
  
agric_c %>% 
  filter(!is.na(buyer)) %>% 
  filter(princ_culture %in% c('Mil', 'Maïs', 'Sorgho')) %>% 
  group_by(region, princ_culture, buyer) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(region = reorder(region, n)) %>% 
  ggplot(aes(fill = buyer, x = princ_culture, y = n)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = '', y = 'Pourcentage', fill = "Principal acheteur") +
  ggtitle("Une part importante de la production locale en denrées de base se vend de gré à gré, hors des marchés") +
  facet_wrap(~region, scales = 'free_x', nrow = 2) +
  theme_minimal_grid() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("outputs/plot_reason_diff_to_sell.png", width = 40, height = 20, units = "cm")




