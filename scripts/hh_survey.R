source("scripts/library.R") # load packages
source("R/functions.R") # load functions

# import road data
ehcvm_individu_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/ehcvm_individu_ner2018.csv")
ehcvm_menage_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/ehcvm_menage_ner2018.csv")
ehcvm_welfare_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/ehcvm_welfare_ner2018.csv")
grappe_gps_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/grappe_gps_ner2018.csv")
s08a_me_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/s08a_me_ner2018.csv")
s08b1_me_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/s08b1_me_ner2018.csv")
s08b2_me_ner2018 <- read_csv("data_raw/NER_2018_EHCVM_v02_M_CSV/s08b2_me_ner2018.csv")

# add primary/foreign keys to tables
survey_ind <- ehcvm_individu_ner2018 %>% 
  mutate(id_adm1 = paste0(country, "00", region),
         id_adm2 = case_when(
           sousregion == 11 ~ "NER001001",
           sousregion == 12 ~ "NER001002",
           sousregion == 13 ~ "NER001003",
           sousregion == 14 ~ "NER001004",
           sousregion == 15 ~ "NER001005",
           sousregion == 16 ~ "NER001006",
           sousregion == 21 ~ "NER002001",
           sousregion == 22 ~ "NER002002",
           sousregion == 23 ~ "NER002003",
           sousregion == 24 ~ "NER002004",
           sousregion == 25 ~ "NER002005",
           sousregion == 26 ~ "NER002006",
           sousregion == 31 ~ "NER003001",
           sousregion == 32 ~ "NER003002",
           sousregion == 33 ~ "NER003003",
           sousregion == 34 ~ "NER003004",
           sousregion == 35 ~ "NER003005",
           sousregion == 36 ~ "NER003006",
           sousregion == 37 ~ "NER003007",
           sousregion == 38 ~ "NER003008",
           sousregion == 41 ~ "NER004001",
           sousregion == 42 ~ "NER004002",
           sousregion == 43 ~ "NER004003",
           sousregion == 44 ~ "NER004004",
           sousregion == 45 ~ "NER004005",
           sousregion == 46 ~ "NER004006",
           sousregion == 47 ~ "NER004008",
           sousregion == 48 ~ "NER004009",
           sousregion == 51 ~ "NER005001",
           sousregion == 52 ~ "NER005002",
           sousregion == 53 ~ "NER005003",
           sousregion == 54 ~ "NER005004",
           sousregion == 55 ~ "NER005005",
           sousregion == 56 ~ "NER005006",
           sousregion == 57 ~ "NER005007",
           sousregion == 58 ~ "NER005008",
           sousregion == 59 ~ "NER005009",
           sousregion == 61 ~ "NER006001",
           sousregion == 62 ~ "NER006002",
           sousregion == 63 ~ "NER006003",
           sousregion == 64 ~ "NER006004",
           sousregion == 65 ~ "NER006005",
           sousregion == 66 ~ "NER006006",
           sousregion == 67 ~ "NER006007",
           sousregion == 68 ~ "NER006008",
           sousregion == 69 ~ "NER006009",
           sousregion == 71 ~ "NER007001",
           sousregion == 72 ~ "NER007002",
           sousregion == 73 ~ "NER007003",
           sousregion == 74 ~ "NER007004",
           sousregion == 75 ~ "NER007005",
           sousregion == 76 ~ "NER007006",
           sousregion == 77 ~ "NER007007",
           sousregion == 78 ~ "NER007008",
           sousregion == 79 ~ "NER007009",
           sousregion == 490 ~ "NER004007",
           sousregion == 510 ~ "NER005010",
           sousregion == 511 ~ "NER005011",
           sousregion == 512 ~ "NER005012",
           sousregion == 590 ~ "NER005013",
           sousregion == 610 ~ "NER006010",
           sousregion == 611 ~ "NER006011",
           sousregion == 612 ~ "NER006012",
           sousregion == 613 ~ "NER006013",
           sousregion == 710 ~ "NER007010",
           sousregion == 790 ~ "NER007011",
           sousregion == 890 ~ "NER008001",
           TRUE ~ as.character(NA)
           ),
         hhid = as_factor(hhid)) %>% 
  relocate(starts_with("id_adm"), .after = sousregion)

# N.B. Five departements don't have any obs. in the HH survey data: Banibangou, Tillia, Bosso,Tassara, and Bilma.
# departements %>% anti_join(survey_ind, by = "id_adm2")

# survey_menage
survey_menage <- ehcvm_menage_ner2018 %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  left_join(survey_ind %>% select(hhid, id_adm1, id_adm2, hhweight) %>% distinct(), by = "hhid", copy = TRUE) %>% 
  relocate(c(starts_with("id_adm"), hhweight), .after = hhid)

# survey_welfare
survey_welfare <- ehcvm_welfare_ner2018 %>% 
  mutate(hhid = as_factor(hhid)) %>% 
  left_join(survey_ind %>% select(hhid, id_adm1, id_adm2) %>% distinct(), by = "hhid", copy = TRUE) %>% 
  relocate(starts_with("id_adm"), .after = hhid)

# food_security
# n.b. there are three tables in Section 8: Food security

add_id_and_wght <- function(df) { # new function which adds identifier var. (hhid, id_adm*) and weight (hhweight) to tables 
  
x <- df %>% 
  mutate(hhid = as_factor( 
    if_else(menage < 10,
            str_insert(paste0(grappe, 0, menage), -1, "0"),
            paste0(grappe, 0, menage))
    )) %>% 
    left_join(survey_ind %>% select(hhid, id_adm1, id_adm2, hhweight) %>% distinct(), by = "hhid", copy = TRUE) %>% 
    relocate(c(hhid, starts_with("id_adm"), hhweight), before = vague)

return(x)

}

food_security_a <- add_id_and_wght(s08a_me_ner2018)
food_security_b1 <- add_id_and_wght(s08b1_me_ner2018)
food_security_b2 <- add_id_and_wght(s08b2_me_ner2018)

# specifying the survey sample design
survey_desgn <- svydesign(id = ~ grappe+hhid, 
          strata = ~ region+zae, 
          weights = ~ hhweight, 
          data = survey_welfare, nest = TRUE)

# save data
save(survey_ind, survey_menage, survey_welfare, survey_desgn,
     food_security_a, food_security_b1, food_security_b2, 
     file = "data/survey.RData")


