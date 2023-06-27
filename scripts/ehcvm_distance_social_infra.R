load(here('data','grappes.RData'))

s02_co_dtance <- read_dta('data_raw/ner_2018_ehcvm/s02_co_ner2018.dta')

# 476 EAs out of 493 w/ GPS coordinates (and 504 EAs in total)
# 1/3 of missing observations for s02q03 (time estimate)

dtance <- s02_co_dtance %>% 
  rowwise() %>% 
  mutate(educ = max(c(s02q01__1, s02q01__2, s02q01__3, s02q01__4))) %>% 
  ungroup() %>% 
  mutate(dtance_educ = if_else(is.na(s02q03), 0, 
                               s02q01__1/s02q03 + s02q01__2/s02q03 + s02q01__3/s02q03 + s02q01__4/s02q03)
         ) %>% 
  select(grappe, s02q01__1:s02q01__4, s02q03, educ, dtance_educ) %>% 
  mutate(dtance_educ_grappe = mean(dtance_educ, na.rm = TRUE), .by = c(grappe, educ))
  
  
ggplot(aes(dtance_educ)) +
  geom_histogram()

  