
## NIGERIA
# import
nga_adm_sf <- st_read(here('data_raw', 'road_network_weight', 'nga_adm_osgof_20190417_em_shp', 'nga_admbnda_adm2_osgof_20190417_em.shp')) %>% 
  clean_names() %>% 
  select(adm_name = adm2_ref, adm_pcode = adm2_pcode, geometry)

nga_admpop_adm2_2020 <- read_excel(here('data_raw', 'road_network_weight', 'nga_admpop_2020.xlsx'), 
                                   sheet = "nga_admpop_adm2_2020") %>% 
  clean_names() %>% 
  select(adm_name = adm2_name, adm_pcode = adm2_pcode, t_tl)

## NIGER
ner_adm_sf <- st_read(here('data_raw', 'road_network_weight', 'ner_adm03_feb2018', 'NER_adm03_feb2018.shp')) %>% 
  clean_names() %>% 
  select(adm_name = adm_02, adm_pcode = rowcacode2, geometry)

ner_admpop_adm2_2022 <- read_csv(here('data_raw', 'road_network_weight', 'ner_admpop_adm3_2022.csv')) %>%
  clean_names() %>%
  select(adm_name = adm2_name, adm_pcode = adm2_pcode, t_tl)

## CHAD
# import
tch_adm_sf <- st_read(here('data_raw', 'road_network_weight', 'tcd_admbnda_adm2', 'tcd_admbnda_adm2_ocha_20170615.shp')) %>% 
  clean_names() %>% 
  select(adm_name = admin2name, adm_pcode = admin2pcod, geometry)

tch_admpop_adm2_2022 <- read_excel(here('data_raw', 'road_network_weight', 'tcd_admpop_adm2_2022.xlsx'),
                                   sheet = "ProjPop21_22_23_admin2", skip = 1) %>%
                                clean_names() %>% 
  select(adm_name = admin2name_fr, adm_pcode = admin2pcode, t_tl = proj_2022_total_16)


## REGION POPULATION
reg_admpop <- rbind(nga_admpop_adm2_2020, ner_admpop_adm2_2022, tch_admpop_adm2_2022) %>% 
  select(-adm_name)

reg_adm_sf <- rbind(nga_adm_sf, ner_adm_sf, tch_adm_sf)

reg_admpop_sf <- left_join(reg_adm_sf, reg_admpop, by = 'adm_pcode')

st_write(reg_admpop_sf, here('data', 'regional_road', 'reg_admpop.shp'))


