load("data/network.RData")
load("data/adm03.RData")

# GIS ADM1 boundaries
adm_zinder <- adm03 %>% select(adm_01, id_adm1, adm_02, id_adm2, geometry) %>% filter(id_adm1 == "NER007")

# Survey welfare
zinder_welfare <- survey_welfare %>%
  filter(id_adm1 == "NER007") %>% 
  mutate(dtot_pc = dtot / hhsize,
         dtot_eqadu1 = dtot / eqadu1,
         dtot_eqadu2 = dtot / eqadu2,
         dali_pc = dali / hhsize,
         dali_eqadu1 = dali / eqadu1,
         dali_eqadu2 = dali / eqadu2,         
         dtot_pov_pc = dtot / (hhsize * def_spa * def_temp),
         dtot_pov_eqadu1 = dtot / (eqadu1 * def_spa * def_temp),
         dtot_pov_eqadu2 = dtot / (eqadu2 * def_spa * def_temp),
         pov_pc = if_else(dtot_pov_pc < zref, 1, 0),
         pov_eqadu1 = if_else(dtot_pov_eqadu1 < zref, 1, 0),
         pov_eqadu2 = if_else(dtot_pov_eqadu2 < zref, 1, 0)
  )

# define the survey design
zinder_srvy <- zinder_welfare %>% 
  as_survey_design(id = c(grappe, hhid), 
                  strata = c(region, zae), 
                  weights = hhweight, 
                  nest = TRUE)

# poverty rate per ADM2
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = zinder_srvy$pct, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  zinder_srvy$adm_02, zinder_srvy$pct) %>% 
  lapply(htmltools::HTML)

zinder_srvy %>% 
  group_by(id_adm2, pov_pc) %>% 
  summarise(pct = 100 * survey_prop()) %>% 
  inner_join(adm_zinder, by = "id_adm2") %>% 
  filter(pov_pc == 1) %>% 
  st_as_sf() %>% 
  leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolygons(
    fillColor = ~pal(pct),
    fillOpacity = 0.7,
    weight = 2,
    opacity = 1,
    color = "white",
    label = ~adm_02
    )

# estimate mean consumption per capita to create new relative poverty variable ()
zinder_srvy %>% 
  summarise(mean_dtot_pc = survey_mean(dtot_pc, vartype = "ci"), 
            mean_pcexp = survey_mean(pcexp, vartype = "ci"),
            quantile = survey_quantile(dtot_pc, c(.1, .25, .5, .75, .9)),
            median = survey_median(dtot_pc, vartype = c("ci"))
            )

leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines(
    data = net_zinder
  )

net_zinder = st_filter(small_edges, adm_zinder)

# Round coordinates to 4 digits.
st_geometry(net_zinder) = st_geometry(net_zinder) %>%
  lapply(function(x) round(x, 4)) %>%
  st_sfc(crs = st_crs(net_zinder))

simple = net_zinder %>%
  activate("edges") %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

subdivision = convert(simple, to_spatial_subdivision)
smoothed = convert(subdivision, to_spatial_smooth)

plot(smoothed)

ways_centrality = smoothed |> 
  activate("edges") |>  
  mutate(betweenness = tidygraph::centrality_edge_betweenness(lengths))

