## v_clean: clean network with the GRASS's v.clean function

v_clean = function(x, directed = FALSE) {

# Add data to GRASS spatial database  
writeVECT(
  SDF = x, 
  vname = 'vect', 
  v.in.ogr_flags = 'overwrite'
)

# Execute the v.clean tool
execGRASS("g.proj", flags = c("c", "quiet"), proj4 = proj4)
execGRASS(
  cmd = 'v.clean', 
  input = 'vect', 
  output = 'vect_cleaned',        
  tool = 'break', 
  flags = c('overwrite', 'c')
)

# Read back into R
use_sf()
lines <- readVECT('vect_cleaned') %>%
  rename(geometry = geom) %>%
  select(-cat)

}

## sf_to_tidygraph: takes a cleaned sf object with LINESTRING geometries as input, and returns a spatial tbl_graph

sf_to_tidygraph = function(x, directed = FALSE) {
  
  # give each edge a unique index
  edges <- name |>
    mutate(edgeID = c(1:n()))
  
  # create nodes at the start and end point of each edge
  nodes <- edges |>
    st_coordinates() |>
    as_tibble() |>
    rename(edgeID = L1) |> # integer indicator specifying to which line a point belongs
    group_by(edgeID) |>
    slice(c(1, n())) |>
    ungroup() |>
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) |> 
  
  # give each node a unique index
  nodes <- nodes |>
    mutate(xy = paste(.$X, .$Y)) |> 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) |> # give each group of unique X,Y-combinations a unique index
    select(-xy)
  
  # combine the node indices with the edges
  source_nodes <- nodes |>
    filter(start_end == 'start') |>
    pull(nodeID)
  
  target_nodes <- nodes |>
    filter(start_end == 'end') |>
    pull(nodeID)
  
  edges = edges |>
    mutate(from = source_nodes, to = target_nodes)
  
  # remove duplicate nodes
  nodes <- nodes |>
    distinct(nodeID, .keep_all = TRUE) |>
    select(-c(edgeID, start_end)) |>
    st_as_sf(coords = c('X', 'Y')) |>
    st_set_crs(st_crs(edges))
  
  # convert to tbl_graph
  graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
}