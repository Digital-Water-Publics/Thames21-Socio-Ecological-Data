####
####
#### AIM: fetch waterbody classificatoin for different waterbodies
####
####

#### READ DATA
wbids = read.csv("data/ea_wbids.csv")

#### FUNCTION TO GET SF FEATURES OF WATERBODY
get_wb_classification = function(string, #### STRING = NAME OF CLASSFICATION AREA
                                 column)
  #### COLUMN  = CLASSIFICATION TYPE E.G. OC | MC | RBD
{
  #### LOGICAL OPERATOR FOR RIVER MINE
  if (column == "OC") {
    wb = wbids %>% subset(OC == string)
  } # OPERATIONAL CATCHMENT
  if (column == "MC") {
    wb = wbids %>% subset(MC == string)
  } # MANAGMENT CATCHMENT
  if (column == "RBD") {
    wb = wbids %>% subset(RBD == string)
  }
  if (column == "WB") {
    wb = wbids %>% subset(WBID == string)
  }# RIVER BASIN DISTRICT
  if (column != "OC" &
      column != "MC" & column != "RBD" & column != "WB") {
    message("Woops, looks like you declared an invalid column type. Please try E.G. OC | MC | RBD")
  } else{
    #### LOOP THROUGH GEOJSON DOWNLOAD
    suppressWarnings(for (i in 1:nrow(wb)) {
      ##### EA CATCHMNET API CALL
      url = "https://environment.data.gov.uk/catchment-planning/WaterBody/"
      notation = wb$WBID[i]
      download_url = paste0(url, notation, "/classifications.csv")

      #### SET OUTPUT PATH
      river_wbid = wb$WBID[i]
      path = "data/river_sf/"
      river_output = paste0(path, river_wbid, ".csv")

      #### DOWNLOOAD FILE, AT LEAST TRY TO
      tryCatch(
        expr = {
          download.file(download_url, river_output)
        },
        error = function(e) {
          message("Unable to download River Please check column and string are correct")
        }
      )

      river_sf = read.csv(river_output)
      names(river_sf)[names(river_sf) == "Water.Body.ID"] = "WBID"

      wb = merge(river_sf, wb)

      #### REMOPVE DOWNLOADED FILE
      file.remove(river_output)
    })
    return(wb)
  }
}

# test function
wb_class = get_wb_classification(string = "GB106039017030", column = "WB")

#### FUNCTION TO GET SF FEATURES OF WATERBODY
get_wb_rnag = function(string, #### STRING = NAME OF CLASSFICATION AREA
                                 column)
  #### COLUMN  = CLASSIFICATION TYPE E.G. OC | MC | RBD
{
  #### LOGICAL OPERATOR FOR RIVER MINE
  if (column == "OC") {
    wb = wbids %>% subset(OC == string)
  } # OPERATIONAL CATCHMENT
  if (column == "MC") {
    wb = wbids %>% subset(MC == string)
  } # MANAGMENT CATCHMENT
  if (column == "RBD") {
    wb = wbids %>% subset(RBD == string)
  }
  if (column == "WB") {
    wb = wbids %>% subset(WBID == string)
  }# RIVER BASIN DISTRICT
  if (column != "OC" &
      column != "MC" & column != "RBD" & column != "WB") {
    message("Woops, looks like you declared an invalid column type. Please try E.G. OC | MC | RBD")
  } else{
    #### LOOP THROUGH GEOJSON DOWNLOAD
    suppressWarnings(for (i in 1:nrow(wb)) {
      ##### EA CATCHMNET API CALL
      url = "https://environment.data.gov.uk/catchment-planning/WaterBody/"
      notation = wb$WBID[i]
      download_url = paste0(url, notation, "/rnags.csv")

      #### SET OUTPUT PATH
      river_wbid = wb$WBID[i]
      path = "data/river_sf/"
      river_output = paste0(path, river_wbid, ".csv")

      #### DOWNLOOAD FILE, AT LEAST TRY TO
      tryCatch(
        expr = {
          download.file(download_url, river_output)
        },
        error = function(e) {
          message("Unable to download River Please check column and string are correct")
        }
      )

      river_sf = read.csv(river_output)
      names(river_sf)[names(river_sf) == "Water.Body.ID"] = "WBID"

      wb = merge(river_sf, wb)

      #### REMOPVE DOWNLOADED FILE
      file.remove(river_output)
    })
    return(wb)
  }
}
