load("data/adm03.RData")
load("data/settlements_id.RData")
load("data/adm3_pop.RData")
load("data/adm3_pop.RData")

# create new key variable to join the datasets at the admin3 level based on postcodes
# Administrative level 3 boundaries
adm03_id <- adm03 %>% 
  mutate(id_adm1 = rowcacode1,
         id_adm2 = rowcacode2,
         id_adm3 = rowcacode3
         )
# Population 
adm3_pop_id <- adm3_pop %>% 
  mutate(id_adm1 = adm1_pcode,
         id_adm2 = adm2_pcode,
         id_adm3 = adm3_pcode
  )

# setdiff(adm03$id_adm3, adm3_pop$id_adm3)

adm03_id %>% 
  

# Join administrative boundaries and population (polygons)
population_admin_id <- adm03_id %>%
  left_join(adm3_pop_id, by = c("id_adm3", "id_adm2", "id_adm1"))

# save data
save(adm03_id, adm3_pop_id, population_admin_id, file = "data/population_admin_id.RData")
save(key_id, file = "data/key_id.RData")
