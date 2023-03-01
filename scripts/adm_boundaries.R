source("scripts/library.R") # load packages

adm00 <- st_read("data_raw/ner_adm00_feb2018/NER_adm00_feb2018.shp")
adm01 <- st_read("data_raw/ner_adm01_feb2018/NER_adm01_feb2018.shp")
adm02 <- st_read("data_raw/ner_adm02_feb2018/NER_adm02_feb2018.shp")
adm03 <- st_read("data_raw/ner_adm03_feb2018/NER_adm03_feb2018.shp")

adm03 <- adm03 %>% 
  clean_names()

# save clean data
save(adm03, file = "data/adm03.RData")