source("script/library.R") # load packages
load("data/population_admin_id.RData")

ipc <- read_excel("data_raw/cadre_harmonise_caf_ipc.xlsx") %>% 
  filter(adm0_pcod2 == "NE") %>% 
  clean_names()

# vis_miss(ipc)
ipc_id <- ipc %>% 
  mutate(id_adm2 = str_insert(str_replace_all(adm2_pcod2, "NE0", "NER00"), 6, "0"), # modify postcode to match id_key format
         id_adm1 = str_replace_all(adm1_pcod2, "NE0", "NER00")) %>% 
  relocate(starts_with("id_adm"), .before = adm1_pcod2) %>% 
  select(!c(contains("_5"), starts_with("fcg_"), starts_with("hdds_"), 
              starts_with("hhs_"), starts_with("lh_hcs"), starts_with("r_csi"),
            starts_with("adm3_"))
         ) %>% 
  mutate(sh_phase35 = (phase35 / population)*100) %>% 
  relocate(sh_phase35, .after = phase35)
  
  
ipc_id <- adm03_id %>% 
  select(id_adm1, id_adm2, geometry, starts_with("adm_")) %>% # check with anti_join
  inner_join(ipc_id, by = c("id_adm1", "id_adm2"))

# save data
save(ipc, ipc_id, file = "data/ipc_id.RData")

# map
tm_shape(adm03_id) +
  tm_polygons() +
tm_shape(ipc_id) +
  tm_polygons("sh_phase35")
  








vis_miss(ipc_clean)
vis_dat(ipc)






%>% 
  select(id_adm2, exercise_label, exercise_year, chtype, reference_label, sh_pahse35, population, phase35)



