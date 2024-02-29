graph <- weight_streetnet (hampi)
from <- sample (graph$from_id, size = 100)
dlim <- c (1, 2, 5, 10, 20) * 100
d <- dodgr_isodists (graph, from = from, dlim)


node_A_major <- graph_sf_centrality |>
  filter(road_type == "major") |>
  pull(from_id) |> 
  unique()


dlim <- c (2, 5, 10, 20) * 100


d <- dodgr_isodists (graph_contr, from = node_A_major, dlim)

# all links with to_id in d are in the catchment
 
# a loop adding up the stats for the ids in `d`

# isochrones are going to overlap....




