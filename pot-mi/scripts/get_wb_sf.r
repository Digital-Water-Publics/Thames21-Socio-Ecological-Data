####
####
#### AIM: fetch waterbody geojson for different waterbody classication groups and transform them into sf objects
####
####
library(sf)
library(dplyr)
#### GET WBID DATA FROM CDE PACKAGE
url = "https://raw.githubusercontent.com/ropensci/cde/master/data-raw/ea_wbids.csv"
download.file(url, "data/ea_wbids.csv")

#### READ DATA
wbids = read.csv("data/ea_wbids.csv")

#### FUNCTION TO GET SF FEATURES OF WATERBODY
get_wb_sf = function(string, #### STRING = NAME OF CLASSFICATION AREA
                     column) #### COLUMN  = CLASSIFICATION TYPE E.G. OC | MC | RBD
  {
  #### LOGICAL OPERATOR FOR RIVER MINE
  if (column == "OC") {wb = wbids %>% filter(OC == string)} # OPERATIONAL CATCHMENT
  if (column == "MC") {wb = wbids %>% filter(MC == string)} # MANAGMENT CATCHMENT
  if (column == "RBD") {wb = wbids %>% filter(RBD == string)} # RIVER BASIN DISTRICT

  #### SET EMPTY DF TO MERGE INTO
  nrows = 1
  wb_sf = st_sf(id = 1:nrows, geometry = st_sfc(lapply(1:nrows, function(x)
    st_geometrycollection())))
  st_crs(wb_sf) = 4326 #### SET CRS TO MATCH THAT OF THE EA
  wb_sf$name = ""
  wb_sf$id = as.character(wb_sf$id)
  wb_sf = wb_sf %>% filter(name == "nun") #### CLEAR ANY VALUES IN DF

  #### LOOP THROUGH GEOJSON DOWNLOAD
  suppressWarnings(
    for (i in 1:nrow(wb)) {
      ##### EA CATCHMNET API CALL
      url = "https://environment.data.gov.uk/catchment-planning/WaterBody/"
      notation = wb$WBID[i]
      download_url = paste0(url, notation, ".geojson")

      #### SET OUTPUT PATH
      river_wbid = wb$WBID[i]
      path = "data/river_sf/"
      river_output = paste0(path, river_wbid, ".geojson")

      #### DOWNLOOAD FILE, AT LEAST TRY TO
      tryCatch(
        expr = {
          download.file(download_url, river_output)
        },
        error = function(e) {
          message("Unable to download River Please check column and string are correct")
        }
      )

      river_sf = read_sf(river_output) %>% select(id, name)

      wb$geometry[i] = river_sf$geometry[1]

      #### REMOPVE DOWNLOADED FILE
      file.remove(river_output)
    }
  )
  return(wb)
}

#### testing 12
thames_sf = get_wb_sf(string = "Thames", column = "RBD")
write_sf(thames_sf, "data/thames_river.geojson")

thames_wb = read_sf("data/thames_river.geojson")
