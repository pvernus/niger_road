source('scripts/library.R') # load packages
source('R/functions.R') # load functions

banque_cerealiere <- st_read(here("data_raw", "wbg_data", "banque_cerealiere", "banque cerealiere.shp")) %>% 
  clean_names()

banque_cerealiere_rn1 <- st_read(here("data_raw", "wbg_data", "banque_cerealiere", "banque cerealiere_rn1.shp")) %>% clean_names()

rt <- st_read(here("data_raw", "wbg_data", "banque_cerealiere", "rt.shp")) %>% 
  clean_names() %>% st_set_crs("WGS84")

rn1_20km_buffer <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "20km_route_nationale_n°1.shp")) %>% clean_names()
rn1_20_km <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "rn1_20km.shp")) %>% clean_names()

rn1 <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "rn1.shp")) %>% clean_names()
route_nationale_1 <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "route nationale n°1.shp")) %>% clean_names()

village_rn1_20km <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "villages_rn1_20km.shp")) %>% clean_names() %>% st_set_crs("WGS84")

infra_agri <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "centre collecte_stock_vente_produit agricole.shp")) %>% clean_names() %>% st_set_crs("WGS84")

infra_hydro <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "infra_hydrau_rn1.shp")) %>% clean_names() %>% st_set_crs("WGS84")

# WARNING
infra_scol <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "infrastructure_scolaire_rn1.shp")) %>% clean_names() %>% st_set_crs(4326)

infra_market_hebdo <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "marches_hebdo_rn1.shp")) %>% clean_names() %>% st_set_crs(4326)

infra_pharma <- st_read(here("data_raw", "wbg_data", "Données de la RN1", "sante_humaine_sante_animale.shp")) %>% clean_names() %>% st_make_valid() %>% st_transform("4326")

st_crs(infra_pharma) = 4326

tmap_mode("view")

tm_shape(infra_hydro) +
  tm_bubbles(col = "type_sourc",
             alpha = .6,
             scale = .1)



tm_shape(banque_cerealiere) +
  tm_bubbles(col = "type_sourc",
             alpha = .3,
             scale = .1) +
tm_shape(banque_cerealiere_rn1) +
  tm_bubbles(col = "blue",
             alpha = .3,
             scale = .1) +
tm_shape(rt) +
  tm_lines(col = "type_route")
