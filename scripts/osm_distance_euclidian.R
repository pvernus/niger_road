source('scripts/library.R') # load packages

distance_euclidian <- read_csv("data/ner_distance_eucl/ner_distance_eucl.csv") %>%
  rename(grappe = cluster) %>% 
  select(-c(latitude, longitude, altitude, yearly_consumption_kg, surface))

distance_euclidian <- rename_with(distance_euclidian, ~ str_remove_all(.x, "_meter"), ends_with("meter")) %>% 
  distinct()
  
  
