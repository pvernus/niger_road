source("scripts/library.R") # load packages
source("R/functions.R") # load functions

## IMPORT DATA

# Administrative boundaries
ner_adm03_feb2018 <- st_read("data_raw/ner_adm03_feb2018/NER_adm03_feb2018.shp")
adm03 <- ner_adm03_feb2018 %>% 
  clean_names() %>% 
  select(-c(objectid, id, iso3, iso2)) %>% # remove unnecessary variables
  mutate(id_adm1 = rowcacode1, # create primary key
         id_adm2 = rowcacode2, # create foreign keys
         id_adm3 = rowcacode3
  )

ner_adm02_feb2018 <- st_read("data_raw/ner_adm03_feb2018/NER_adm03_feb2018.shp")
ner_adm02_feb2018 <- st_make_valid(ner_adm02_feb2018)
adm02 <- ner_adm02_feb2018 %>% 
  clean_names() %>% 
  select(-c(objectid, iso3, iso2)) %>% # remove unnecessary variables
  mutate(id_adm1 = rowcacode1, # create primary key
         id_adm2 = rowcacode2 # create foreign keys
  ) %>% 
  relocate(starts_with("id_"), .before = rowcacode1)
  
regions <- adm03 %>% 
  select(adm_01, id_adm1) %>% 
  st_drop_geometry() %>% distinct()

departements <- adm03 %>% 
  select(adm_01, id_adm1, adm_02, id_adm2) %>% 
  st_drop_geometry() %>% distinct()

# Population
ner_admpop_adm3_2022 <- read_csv(here("data_raw", "ner_admpop_adm3_2022.csv"))
pop <- ner_admpop_adm3_2022 %>% 
  clean_names() %>% 
  select(-c(year, iso3, adm0_name, adm0_pcode)) %>% # remove unnecessary variables
  mutate(id_adm1 = adm1_pcode, # create primary key
         id_adm2 = adm2_pcode, # create foreign keys
         id_adm3 = adm3_pcode
  ) %>% 
  relocate(starts_with("id_"), .before = "f_tl")

pop_sf <- pop %>% 
  inner_join(adm03 %>% select(adm_03, id_adm3, geometry), by = "id_adm3") %>% 
  st_as_sf()


# Settlements
localites <- st_read("data_raw/ner_localites-21-10/Localités 21-10/Localites.gpkg", layer = "REACH_Localites")
settlements <- localites %>% 
  clean_names() %>% 
  filter(type %in% c("CPT", "VA", "H")) %>%  # we leave camp sites and water points
  mutate(id_adm3 = admin3pcode, # create primary key
         id_adm2 = str_sub(id_adm3, 1, 9), # create foreign keys
         id_adm1 = str_sub(id_adm3, 1, 6),
         type = case_when(
           type == "CPT" ~ "urban_neighborhood",
           type == "VA" ~ "village",
           type == "H" ~ "hamlet",
           .default = NA_character_
         ),
         area = if_else(type == "urban_neighborhood", 1, 2)) %>% # create new variable
  rename(long = longitude_x, lat = latitude_y, geometry = geom)

# Food security - IPC
cadre_harmonise_caf_ipc <- read_excel("data_raw/cadre_harmonise_caf_ipc.xlsx") %>% 
  filter(adm0_pcod2 == "NE")
ipc <- cadre_harmonise_caf_ipc %>% 
  clean_names() %>% 
  select(-c(starts_with("adm0"), starts_with("adm1_5"), starts_with("adm2_5"), 
            adm3_name, adm3_pcod2, 
            mortality_phase:notes)) %>% # missing variables are concentrated before 2022
  mutate(id_adm2 = str_insert(str_replace_all(adm2_pcod2, "NE0", "NER00"), 6, "0"), # str_insert is an ad hoc function (cf. R/functions.R) that inserts a character in a string
         id_adm1 = str_replace_all(adm1_pcod2, "NE0", "NER00")) %>% 
  relocate(starts_with("id_adm"), .before = adm1_pcod2)

# Roads
source("scripts/road_network.R")

# HH survey
source("scripts/hh_survey.R")


## STOCK DATA

# A. save as RData
save(ner_adm03_feb2018, adm03, 
     ner_adm02_feb2018, adm02,
     regions, departements,
     ner_admpop_adm3_2022, pop, pop_sf,
     localites, settlements,
     cadre_harmonise_caf_ipc, ipc,
     survey_ind, survey_menage, survey_welfare, survey_desgn,
     food_security_a, food_security_b1, food_security_b2,
     file = "data/data.RData")

# B. create new database
con <- DBI::dbConnect(RSQLite::SQLite(), "data/niger_data.db")
# add tables to the database
dbWriteTable(con, "adm03", adm03, overwrite = TRUE)
dbWriteTable(con, "regions", regions, overwrite = TRUE)
dbWriteTable(con, "departements", departements, overwrite = TRUE)
dbWriteTable(con, "pop", pop, overwrite = TRUE)
dbWriteTable(con, "settlements", settlements, overwrite = TRUE)
dbWriteTable(con, "ipc", ipc, overwrite = TRUE)
dbWriteTable(con, "survey_ind", ipc, overwrite = TRUE)
dbWriteTable(con, "survey_menage", ipc, overwrite = TRUE)
dbWriteTable(con, "survey_welfare", ipc, overwrite = TRUE)
dbWriteTable(con, "food_security_a", ipc, overwrite = TRUE)
dbWriteTable(con, "food_security_b1", ipc, overwrite = TRUE)
dbWriteTable(con, "food_security_b2", ipc, overwrite = TRUE)
dbDisconnect(con)
