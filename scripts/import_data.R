source("scripts/library.R") # load packages
source("R/functions.R") # load functions

## IMPORT DATA

# Administrative boundaries
# from the rhdx package
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

search_datasets("niger adm03 administrative boundaries-divisions 2018", rows = 5)

ner_adm00 <- pull_dataset("cod-ab-ner") %>%
  get_resource(2) %>%
  read_resource()
ner_adm01 <- pull_dataset("cod-ab-ner") %>%
  get_resource(3) %>%
  read_resource()
ner_adm02 <- pull_dataset("cod-ab-ner") %>%
  get_resource(4) %>%
  read_resource()
ner_adm03 <- pull_dataset("cod-ab-ner") %>%
  get_resource(5) %>%
  read_resource()

ner_adm03 <- st_make_valid(ner_adm03)
adm03 <- ner_adm03 %>% 
  clean_names() %>% 
  select(-c(objectid, id, iso3, iso2)) %>% # remove unnecessary variables
  mutate(id_adm1 = rowcacode1, # create primary key
         id_adm2 = rowcacode2, # create foreign keys
         id_adm3 = rowcacode3
  )

ner_adm02 <- st_make_valid(ner_adm02)
adm02 <- ner_adm02 %>% 
  clean_names() %>% 
  select(-c(objectid, iso3, iso2)) %>% # remove unnecessary variables
  mutate(id_adm1 = rowcacode1, # create primary key
         id_adm2 = rowcacode2 # create foreign keys
  ) %>% 
  relocate(starts_with("id_"), .before = rowcacode1)
  
save(ner_adm00, ner_adm01, ner_adm02, ner_adm03, adm02, adm03, file = "data/ner_adm.RData")

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


# GRID3
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()
search_datasets("GRID3 Niger Settlement Extents, Version 01.01", rows = 5)

grid3_settlement <- pull_dataset("grid3-niger-settlement-extents-version-01-01") %>%
  get_resource(1) %>%
  read_resource() %>% 
  clean_names() %>% 
  mutate(id_adm1 = adm1_pcode, # create primary key
         id_adm2 = adm2_pcode, # create foreign keys
         id_adm3 = adm3_pcode
  ) %>% 
  relocate(starts_with("id_"), .before = "adm1_pcode")

save(grid3_settlement, file = "data/grid3_settlement.RData")

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
     grid3_ner_settlement,
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
