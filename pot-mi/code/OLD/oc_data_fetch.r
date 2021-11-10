#### READ NATIONAL OC DATA #todo make this dynamic
england_oc = read.csv("../../../../Downloads/OperationalCatchment-.csv") %>% select(label,
                                                                                    notation,
                                                                                    inManagementCatchment.label,
                                                                                    inRiverBasinDistrict.label)

#### FUNCTION takes notation and returns
mc_sf = function(id) {
  #### GET DATA FROM EA CATCHMENT PLANNING
  url = "https://environment.data.gov.uk/catchment-planning/so/OperationalCatchment/"
  notation = id
  download_url = paste0(url, notation, "/polygon")

  #### GET DF labels by filtering england data
  river_filter = england_oc %>% filter(notation == id)
  label = str_replace_all(river_filter$label, " ", "_")
  managment_catchment = river_filter$inManagementCatchment.label
  river_basin_district = river_filter$inRiverBasinDistrict.label

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

#### GET THAMES DATAs
thames_oc = england_oc %>% filter(inRiverBasinDistrict.label == "Thames")

for(i in 1:nrow(thames_oc)){
  mc_sf(id = thames_oc$notation[i])
}

thames_oc_sf = rbind(
  Aldermaston_Bagshot_Beds,
  Alton_Chalk,
  Banbury_Jurassic,
  Basingstoke_Chalk,
  Berkshire_Downs_Chalk,
  Beult,
  Beverley_Brook,
  `Bicester-Otmoor_Cornbrash`,
  Brent_Canals_and_SWT,
  Brent_Rivers_and_Lakes,
  Burford_Jurassic,
  Byfield_Jurassic,
  Cherwell,
  Cherwell_Canals_and_SWT,
  Chiltern_Chalk_Scarp,
  Chilterns_South,
  `Chilterns_South-West_Chalk`,
  Chipping_Norton_Jurassic,
  Chobham_Bagshot_Beds,
  Colne,
  Colne_Canals_and_SWT,
  Colne_GW,
  Crane_Rivers_and_Lakes,
  Crane_SWT,
  Cray_and_Shuttle,
  Darent,
  Eden,
  Effingham_Tertiaries,
  Epsom_and_Dorking_Chalk,
  Essex_South_Thurrock_Chalk,
  Essex_South_Lower_London_Tertiaries,
  Evenlode,
  Farnborough_Bagshot_Beds,
  Godalming_Lower_Greensand,
  Greenwich_Tertiaries,
  Guildford_Chalk,
  Headington_Corallian,
  Hogsmill,
  Kemble_Forest_Marble,
  Kennet,
  Kennet_Canals,
  Kent_Greensand_Middle_and_Western,
  Kent_North_Chalk_and_Tertiaries,
  Kent_North_Medway_Chalk,
  Lee_Lower_Rivers_and_Lakes,
  Lee_Upper,
  Lee_Lower_Canals_and_SWT,
  Lee_Upper_GW,
  Loddon,
  Loddon_Canals,
  Maidenhead_Chalk,
  Mardyke,
  Marsh_Dykes,
  Medway_Lower,
  Medway_Middle,
  Medway_Swale_Estuary,
  Medway_Upper,
  Mole_Lower_and_Rythe,
  Mole_Upper_Trib,
  Ock,
  Old_Basing_Tertiaries,
  Oxon_Ray,
  Ravensbourne,
  Reigate_Lower_Greensand,
  Roding_Beam_and_Ingrebourne,
  Shrivenham_Corallian,
  Tackley_Jurassic,
  Teise,
  Thame,
  Thame_Canals,
  Thames_Lower,
  Thames_Lower_SWT,
  Thames_Upper,
  Thames_Upper_Gravels,
  Thatcham_Tertiaries,
  Tidal_Thames,
  `Tunbridge_Wells_Sand_and_Kent_Weald_-_Thames`,
  Twyford_Tertiaries,
  Vale_of_White_Horse_Chalk,
  Wandle,
  West_Kent_and_Bromley_Tertiaries,
  West_Kent_Darent_and_Cray_Chalk,
  Wey,
  White_Drain_and_Lakes,
  Windrush
)
