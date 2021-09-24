england_oc = read.csv("../../../../Downloads/OperationalCatchment-.csv") %>% select(label,
                                                                                    notation,
                                                                                    inManagementCatchment.label,
                                                                                    inRiverBasinDistrict.label)

get_thames_mc_sf = function(notation, label) {
  #### GET DATA FROM EA CATCHMENT PLANNING
  url = "https://environment.data.gov.uk/catchment-planning/so/OperationalCatchment/"
  notation = notation
  download_url = paste0(url, notation, "/polygon")


  #### SET OUTPUT PATH
  river_name = label
  path = "data/oc_sf/"
  river_output = paste0(path, river_name, ".geojson")

  #### DOWNLOOAD FILE
  tryCatch(
    expr = {
      download.file(download_url, river_output)
    },
    error = function(e) {
      message("Unable to download OC. Please check label and notation are correct")
    }
  )

  #### GET DF labels by filtering england data
  river_filter = england_oc %>% filter(england_oc$notation == notation_t)
  label = str_replace_all(river_filter$label, " ", "_")
  managment_catchment = river_filter$inManagementCatchment.label
  river_basin_district = river_filter$inRiverBasinDistrict.label

  #### SET DATAFRAME LABELS
  river_sf = read_sf(river_output) %>%
    mutate(notation = notation) %>%
    mutate(label = label) %>%
    mutate(managment_catchment = managment_catchment) %>%
    mutate(river_basin_district = river_basin_district)

  #### REMOPVE DOWNLOADED FILE
  file.remove(river_output)

  #### WRITE SF FILE
  write_sf(river_sf, river_output)

  #### UPDATE ON PROGRESS
  print(paste0(label, " has been fetched and saved as an sf object"))

  #### Assign as global variable
  assign(label, river_sf, envir = .GlobalEnv)

}
