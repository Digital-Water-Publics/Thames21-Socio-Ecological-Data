#### READ DATA file
thames_mc = read.csv("../../../../Downloads/management-catchments-inRiverBasinDistrict-6.csv") %>%
  select(label,notation)


get_thames_mc_sf = function(notation,label){

  #### GET DATA FROM EA CATCHMENT PLANNING
  url = "https://environment.data.gov.uk/catchment-planning/so/ManagementCatchment/"
  notation = notation
  download_url = paste0(url,notation,"/polygon")

  #### SET OUTPUT PATH
  river_name = label
  path = "data/mc_sf/"
  river_output = paste0(path,river_name,".geojson")

  #### DOWNLOOAD FILE
  tryCatch(
    expr = {
      download.file(download_url, river_output)
    },
    error = function(e){
      message("Unable to download MC. Please check label and notation are correct")
    }
  )

  #### SET DATAFRAME LABELS
  label = str_replace_all(label, " ", "_")
  river_sf = read_sf(river_output) %>%
    mutate(notation = notation) %>%
    mutate(label = label)

  #### REMOPVE DOWNLOADED FILE
  file.remove(river_output)

  #### WRITE SF FILE
  write_sf(river_sf, river_output)

  #### UPDATE ON PROGRESS
  print(paste0(label, " has been fetched and saved as an sf object"))
  assign(label, river_sf, envir = .GlobalEnv)

}
if(file.exists("data/mc_sf/thames_mc.geojson")){
  thames_mc_sf = read_sf("data/mc_sf/thames_mc.geojson")
} else {
  ##### LOOP OVER MC DATAFRAME
  for (i in 1:nrow(thames_mc)) {
    get_thames_mc_sf(notation = thames_mc$notation[i], label = thames_mc$label[i])
  }
  #### BIND SF
  thames_mc_sf = rbind(
    Cherwell_and_Ray,
    Colne,
    Cotswolds,
    Darent_and_Cray,
    Essex_South,
    Gloucestershire_and_the_Vale,
    Kennet_and_Trib,
    Kent_North,
    Lee_Upper,
    London,
    Loddon_and_Trib,
    Maidenhead_and_Sunbury,
    Medway,
    Mole,
    Roding_Beam_and_Ingrebourne,
    Thames_and_Chilterns_South,
    Thames_AWB,
    Thames_GW,
    Thames_TraC,
    Wey_and_Trib
  )
}

for(i in 1:nrow(thames_mc_sf)){
  thames_mc_sf$val[i] = runif(1)
}


write_sf(thames_mc_sf,"data/mc_test.geojson")
write_sf(thames_oc_sf,"data/oc_test.geojson")






