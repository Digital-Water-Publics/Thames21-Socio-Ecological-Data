####
####
#### AIM: fetch waterbody geojson for different waterbody classication groups and transform them into sf objects
####
####

#### GET WBID DATA FROM CDE PACKAGE
url = "https://raw.githubusercontent.com/ropensci/cde/master/data-raw/ea_wbids.csv"
download.file(url, "data/ea_wbids.csv")

#### READ DATA
wbids = read.csv("data/ea_wbids.csv")

#### FUNCTION PURPOSE: FETCH WATERRBODY GEOJSON FROM EA CATCHMENT API
#### FUNCTION RETURNS A SF OBJECT WITH THE FOLLOWING COLUMNS:
##  "WBID"     "name"     "type"     "OC"       "OC_num"   "MC"       "MC_num"   "RBD"      "RBD_num"  "geometry"
## DEPENDING ON THE AREA SPECIFIED, THE FUNCTION WILL REUTRN MBETWEEN 1-x WATERBODIES
get_wb_sf = function(string, #### STRING = NAME OF CLASSFICATION AREA E.G. RIVER TILL
                     column) #### COLUMN  = CLASSIFICATION TYPE E.G. OC | MC | RBD
  {
  #### LOGICAL OPERATOR FOR RIVER MINE
  if (column == "OC") {wb = wbids %>% subset(OC == string)} # OPERATIONAL CATCHMENT
  if (column == "MC") {wb = wbids %>% subset(MC == string)} # MANAGMENT CATCHMENT
  if (column == "RBD") {wb = wbids %>% subset(RBD == string)} # RIVER BASIN DISTRICT
  if(column != "OC" & column != "MC" & column != "RBD"){
    message("Woops, looks like you declared an invalid column type. Please try E.G. OC | MC | RBD")
  } else{
    message("Running function:")
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

        river_sf = read_sf(river_output)
        river_sf = river_sf[,"id","name"]

        wb$geometry[i] = river_sf$geometry[1]

        #### REMOPVE DOWNLOADED FILE
        file.remove(river_output)
      }
    )
    return(wb)
  }
}

#### testing 12
thames_sf = get_wb_sf(string = "Thames", column = "RBD")
# write_sf(thames_sf, "data/thames_river.geojson")
