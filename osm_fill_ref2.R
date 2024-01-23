osm_fill_ref2 <- function(lines,
                          major_ref = c("motorway",
                                        "motorway_link",
                                        "primary",
                                        "primary_link",
                                        "trunk",
                                        "trunk_link")) {
  lines$road_type <-
    dplyr::if_else(lines$highway %in% major_ref, "major", "minor")
  
  
  lines_major <- lines[lines$road_type == "major",]
  lines_minor <- lines[lines$road_type == "minor",]
  
  lines_major$highway <-
    gsub(pattern = "_link$",
         replacement = "",
         lines_major$highway)
  
  
  graph <- dodgr::weight_streetnet(
    lines,
    wt_profile = "motorcar",
    keep_cols = c("name",
                  "ref",
                  "highway",
                  "junction",
                  "road_type")
  )
  
  df_graph <- graph |>
    dodgr_to_sf() |>
    st_drop_geometry() |>
    select(from_id, to_id, highway, way_id, name, ref, junction)
  
  i <- 1
  check_len <- TRUE
  max_iter <- 10
  
  while (i <= max_iter & check_len) {
    print(i)
    major_na_ids <-
      df_graph$way_id[(is.na(df_graph$name) |
                         is.na(df_graph$ref)) &
                        df_graph$way_id %in% lines_major$osm_id] |>
      unique()
    
    lapply(major_na_ids,
           function(t_id) {
             t_nodes <-
               df_graph[df_graph$way_id == t_id, c("from_id", "to_id")] |>
               as.matrix() |>
               as.vector() |>
               unique()
             
             t_fill <- df_graph |>
               dplyr::filter(from_id %in% t_nodes |
                               to_id %in% t_nodes,
                             !(way_id %in% major_na_ids)) |>
               dplyr::summarise(occur = dplyr::n(), .by = c("name", "ref")) |>
               tidyr::drop_na() |>
               dplyr::arrange(dplyr::desc(dplyr::pick("occur"))) |>
               head(n = 1)
             
             if (nrow(t_fill) > 0) {
               if (is.na(unique(df_graph$name[df_graph$way_id == t_id]))) {
                 df_graph$name[df_graph$way_id == t_id] <<- t_fill$name
                 lines_major$name[lines_major$osm_id == t_id] <<-
                   t_fill$name
               }
               
               if (is.na(unique(df_graph$ref[df_graph$way_id == t_id]))) {
                 df_graph$ref[df_graph$way_id == t_id] <<- t_fill$ref
                 lines_major$ref[lines_major$osm_id == t_id] <<-
                   t_fill$ref
               }
             }
             return(NULL)
           })
    
    i = i + 1
    check_len <- length(major_na_ids) > 0
  }
  
  lines = rbind(lines_major, lines_minor)
  
  return(lines)
}
