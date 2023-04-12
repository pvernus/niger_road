
# Inserts a pattern at a specific position in string, see: https://statisticsglobe.com/insert-character-pattern-in-string-r
str_insert <- function(x, pos, insert) {       
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}


## convert sf line object to tbl_graph
# give each edge a unique index

sf_to_tidygraph = function(x, directed = FALSE, force = FALSE) {
  
edges <- x %>%
    mutate(edgeID = c(1:n()))
  
nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    transform(nodeID = as.numeric(factor(xy))) # Create ID by group
  
source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
edges <- edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}


sf_to_sfnetwork = function(x, directed = FALSE, force = FALSE) {
  
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    transform(nodeID = as.numeric(factor(xy))) # Create ID by group
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges <- edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
sfnetwork(nodes = nodes, edges = as_tibble(edges), directed = directed, force = force)
  
}


# 

st_reason_invalid = function(sf) {
  
x <- st_is_valid(sf, reason = TRUE)
x[x != "Valid Geometry"]

}

# Save as single self-contained HTML file

widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
